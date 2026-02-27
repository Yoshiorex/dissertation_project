{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, decode, encode, withObject, (.:), (.:?))
import Data.Text (Text, unpack, pack, replace)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map.Strict as M
import Data.List (isPrefixOf, intercalate, groupBy, sort)
import Data.Char (isDigit, toLower)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Control.Monad.State.Strict (State, get, put, modify, evalState, when)
import Control.Monad (forM_, unless)
import System.IO (hSetBuffering, BufferMode(LineBuffering), stdout, stdin, hIsEOF)
import qualified Data.Yaml as Yaml

-- ==============================
-- 1. TYPES (Haskell's Strength)
-- ==============================

-- Algebraic Data Type for annotations (instead of string matching)
data Annotation
  = Log [Text]
  | Name Text
  | Init
  | Def Text Text
  | Decl Text Text (Maybe Text)
  | DclArray Text Text Text
  | Task Text
  | Signal Text
  | Wait Text
  | State Text Text
  | Scalar Text (Maybe Text) Text  -- name, optional index, value
  | ScalarUnderscore Text
  | Ptr Text Text
  | Struct Text
  | Seq Text
  | End Text
  | Call Text [Text]
  deriving (Show, Eq)

-- Configuration (immutable, parsed once)
data Config = Config
  { templates :: M.Map Text Text       -- Template store (ref_dict)
  , flags :: Flags                     -- Output flags
  , language :: Text                   -- Target language
  , commentSyntax :: CommentSyntax     -- Comment markers
  , segmentInfo :: SegmentInfo         -- Code segment templates
  } deriving (Show)

data Flags = Flags
  { outputLOG :: Bool
  , annoteComments :: Bool
  , outputSWITCH :: Bool
  } deriving (Show, Generic)

data CommentSyntax = CommentSyntax
  { eolc :: Text
  , cmtbegin :: Text
  , cmtend :: Text
  } deriving (Show, Generic)

data SegmentInfo = SegmentInfo
  { segnamepfx :: Text
  , segarg :: Text
  , segdecl :: Text
  , segbegin :: Text
  , segend :: Text
  } deriving (Show, Generic)

-- Processing state (changes over time, but immutably)
data ProcessingState = ProcessingState
  { inSTRUCT :: Bool
  , inSEQ :: Bool
  , inName :: Text
  , seqForSEQ :: Text
  , defCode :: [Text]
  , declCode :: [Text]
  , testCodes :: [(Int, [Text])]  -- (pid, lines)
  , procIds :: [Int]              -- Set would be better, but keeping simple
  , currentId :: Int
  , switchNo :: Int
  } deriving (Show)

-- ==============================
-- 2. PURE FUNCTIONS
-- ==============================

-- Parse a line into an Annotation ADT
parseAnnotation :: [Text] -> Maybe Annotation
parseAnnotation = \case
  [] -> Nothing
  pid:rest -> case rest of
    ["LOG"] -> Just (Log [])
    ["LOG", txt] -> Just (Log [txt])
    "LOG":args -> Just (Log args)
    ["NAME", name] -> Just (Name name)
    ["INIT"] -> Just Init
    ["DEF", name, value] -> Just (Def name value)
    ["DECL", typ, name] -> Just (Decl typ name Nothing)
    ["DECL", typ, name, value] -> Just (Decl typ name (Just value))
    ["DCLARRAY", typ, name, value] -> Just (DclArray typ name value)
    ["TASK", name] -> Just (Task name)
    ["SIGNAL", value] -> Just (Signal value)
    ["WAIT", value] -> Just (Wait value)
    ["STATE", tid, state] -> Just (State tid state)
    ["SCALAR", "_", value] -> Just (ScalarUnderscore value)
    ["SCALAR", name, value] -> Just (Scalar name Nothing value)
    ["SCALAR", name, index, value] -> Just (Scalar name (Just index) value)
    ["PTR", name, value] -> Just (Ptr name value)
    ["STRUCT", name] -> Just (Struct name)
    ["SEQ", name] -> Just (Seq name)
    ["END", name] -> Just (End name)
    "CALL":name:args -> Just (Call name args)
    _ -> Nothing
  where
    readPid :: Text -> Int
    readPid = read . unpack

-- Pure template substitution
substituteTemplate :: Text -> [Text] -> Text
substituteTemplate template args = foldl' substitute template (zip [0..] args)
  where
    substitute :: Text -> (Int, Text) -> Text
    substitute t (i, arg) = replace (pack ("{" ++ show i ++ "}")) arg t

-- Process one annotation (pure!)
processAnnotation :: Config -> ProcessingState -> Annotation -> ProcessingState
processAnnotation config state@ProcessingState{..} ann = case ann of
  Log args -> if outputLOG (flags config)
              then let line = "T_log(T_NORMAL," <> T.intercalate " " args <> ");"
                   in state { testCodes = (currentId, [line]) : testCodes }
              else state
  
  Name name -> lookupAndAdd name [] config state
  
  Init -> lookupAndAdd "INIT" [] config state
  
  Task name -> lookupAndAdd name [] config state
  
  Signal value -> lookupAndAdd "SIGNAL" [value] config state
  
  Wait value -> lookupAndAdd "WAIT" [value] config state
  
  Def name value -> state { defCode = ("#define " <> name <> " " <> value) : defCode }
  
  Decl typ name Nothing -> addDecl name typ Nothing config state
  Decl typ name (Just value) -> addDecl name typ (Just value) config state
  
  DclArray typ name value -> addDclArray name typ value config state
  
  Ptr name value -> addPtr name value inSTRUCT config state
  
  Struct name -> state { inSTRUCT = True, inName = name }
  
  Seq name -> state { inSEQ = True, inName = name, seqForSEQ = "" }
  
  End name -> handleEnd name inSTRUCT inSEQ seqForSEQ config state
  
  ScalarUnderscore value -> if inSEQ
                            then state { seqForSEQ = seqForSEQ <> " " <> value }
                            else state
  
  Scalar name mIndex value -> addScalar name mIndex value inSTRUCT inName config state
  
  State tid stateName -> lookupAndAdd stateName [tid] config state
  
  Call name args -> lookupAndAdd name args config state
  
  where
    addCode pid lines = state { testCodes = (pid, lines) : testCodes }
    
    lookupAndAdd :: Text -> [Text] -> Config -> ProcessingState -> ProcessingState
    lookupAndAdd key args Config{..} s = case M.lookup key templates of
      Nothing -> addCode currentId [eolc commentSyntax <> " CANNOT REFINE '" <> key <> "'"] s
      Just template -> let code = substituteTemplate template args
                       in addCode currentId (T.lines code) s
    
    addDecl :: Text -> Text -> Maybe Text -> Config -> ProcessingState -> ProcessingState
    addDecl name typ mValue Config{..} s = case M.lookup (name <> "_DCL") templates of
      Nothing -> let decl = eolc commentSyntax <> " CANNOT REFINE Decl " <> name <> "_DCL"
                 in if currentId == 0
                    then s { declCode = ("static " <> decl) : declCode }
                    else addCode currentId [decl] s
      Just template -> let baseDecl = substituteTemplate template [name]
                           fullDecl = case mValue of
                                        Nothing -> baseDecl <> ";"
                                        Just v -> baseDecl <> " = " <> v <> ";"
                           finalDecl = if currentId == 0 then "static " <> fullDecl else fullDecl
                       in if currentId == 0
                          then s { declCode = finalDecl : declCode }
                          else addCode currentId [finalDecl] s

-- ==============================
-- 3. STATE MANAGEMENT (Pure Folds)
-- ==============================

-- Process a line with state transitions
processLine :: Config -> ProcessingState -> Text -> ProcessingState
processLine config state line =
  let parts = T.words line
      pid = if not (null parts) then read (unpack (head parts)) else 0
      state' = collectPid pid state
      state'' = if outputSWITCH (flags config) then switchIfRequired pid config state' else state'
      state''' = if annoteComments (flags config) then logLine line config state'' else state''
  in case parseAnnotation parts of
       Just ann -> processAnnotation config state''' ann
       Nothing -> state'''

-- Helper functions (still pure!)
collectPid :: Int -> ProcessingState -> ProcessingState
collectPid pid state@ProcessingState{..} =
  if pid `notElem` procIds
  then state { procIds = pid : procIds }
  else state

switchIfRequired :: Int -> Config -> ProcessingState -> ProcessingState
switchIfRequired pid config state@ProcessingState{currentId, switchNo}
  | pid == currentId = state
  | otherwise = 
      let suspendCode = fromMaybe (eolc (commentSyntax config) <> " SUSPEND: no refinement entry found")
                    (M.lookup "SUSPEND" (templates config))
          wakeupCode = fromMaybe (eolc (commentSyntax config) <> " WAKEUP: no refinement entry found")
                    (M.lookup "WAKEUP" (templates config))
          suspendLine = substituteTemplate suspendCode [pack (show (switchNo + 1)), 
                                                       pack (show currentId), 
                                                       pack (show pid)]
          wakeupLine = substituteTemplate wakeupCode [pack (show (switchNo + 1)), 
                                                     pack (show pid), 
                                                     pack (show currentId)]
      in state { switchNo = switchNo + 1
               , testCodes = (currentId, [suspendLine]) : (pid, [wakeupLine]) : testCodes state
               , currentId = pid }

logLine :: Text -> Config -> ProcessingState -> ProcessingState
logLine line config state = 
  let parts = T.words line
      pid = if length parts > 0 then read (unpack (head parts)) else 0
      logMsg = eolc (commentSyntax config) <> " @@@ " <> line
  in if length parts > 1
     then let keyword = parts !! 1
          in case keyword of
               "NAME" -> state { defCode = logMsg : defCode state }
               "DEF"  -> state { defCode = logMsg : defCode state }
               "DECL" -> if pid == 0
                        then state { declCode = logMsg : declCode state }
                        else addCode pid [logMsg] state
               "DCLARRAY" -> if pid == 0
                            then state { declCode = logMsg : declCode state }
                            else addCode pid [logMsg] state
               "LOG" -> state  -- Skip logging LOG annotations
               _ -> addCode pid ["T_log(T_NORMAL,\"@@@ " <> line <> "\");"] state
     else state
  where
    addCode pid lines = state { testCodes = (pid, lines) : testCodes state }

-- ==============================
-- 4. CONFIGURATION LOADING
-- ==============================

-- JSON instances
instance FromJSON Flags where
  parseJSON = withObject "Flags" $ \v -> Flags
    <$> v .: "outputLOG"
    <*> v .: "annoteComments"
    <*> v .: "outputSWITCH"

instance FromJSON CommentSyntax where
  parseJSON = withObject "CommentSyntax" $ \v -> CommentSyntax
    <$> v .: "EOLC"
    <*> v .: "CMTBEGIN"
    <*> v .: "CMTEND"

instance FromJSON SegmentInfo where
  parseJSON = withObject "SegmentInfo" $ \v -> SegmentInfo
    <$> v .: "SEGNAMEPFX"
    <*> v .: "SEGARG"
    <*> v .: "SEGDECL"
    <*> v .: "SEGBEGIN"
    <*> v .: "SEGEND"

loadLanguageConfig :: Text -> IO (Either String (CommentSyntax, SegmentInfo))
loadLanguageConfig lang = do
  let filename = "languages/" <> unpack (T.toLower lang) <> ".yml"
  result <- Yaml.decodeFileEither filename
  case result of
    Left err -> return $ Left (show err)
    Right yaml -> case (Yaml.parseEither parseYaml yaml) of
      Left err' -> return $ Left err'
      Right cfg -> return $ Right cfg
  where
    parseYaml = withObject "LanguageYaml" $ \v -> do
      comments <- CommentSyntax
        <$> v .: "EOLC"
        <*> v .: "CMTBEGIN"
        <*> v .: "CMTEND"
      segment <- SegmentInfo
        <$> v .: "SEGNAMEPFX"
        <*> v .: "SEGARG"
        <*> v .: "SEGDECL"
        <*> v .: "SEGBEGIN"
        <*> v .: "SEGEND"
      return (comments, segment)

parseConfigFromJson :: Text -> IO (Either String Config)
parseConfigFromJson jsonText = do
  case decode (B.fromStrict (encodeUtf8 jsonText)) of
    Nothing -> return $ Left "Failed to parse JSON"
    Just (Object obj) -> do
      refDict <- case obj M.!? "ref_dict" of
        Just (Object rd) -> return $ M.fromList 
          [(k, v) | (k, String v) <- M.toList rd]
        _ -> return $ M.empty
      
      flags <- case obj M.!? "flags" of
        Just (Object f) -> case fromJSON (Object f) of
          Success fl -> return fl
          Error err -> return $ Flags False True True
        _ -> return $ Flags False True True
      
      language <- case M.lookup "LANGUAGE" refDict of
        Just lang -> return $ T.toLower lang
        Nothing -> return "c"
      
      langConfig <- loadLanguageConfig language
      case langConfig of
        Left err -> return $ Left $ "Language config error: " ++ err
        Right (comments, segment) -> 
          return $ Right $ Config refDict flags language comments segment

-- ==============================
-- 5. CODE GENERATION (Pure)
-- ==============================

generateCode :: Config -> ProcessingState -> Text
generateCode Config{..} ProcessingState{..} =
  let header = eolc commentSyntax <> "  ===============================================\n\n"
      defSection = T.intercalate "\n" (reverse defCode)
      declSection = T.intercalate "\n" (reverse declCode)
      
      -- Group test codes by pid
      groupedCodes = M.fromListWith (++) testCodes
      sortedPids = sort (M.keys groupedCodes)
      
      testSections = T.intercalate "\n" $ map (\pid ->
        let testseg_name = substituteTemplate (segnamepfx segmentInfo) [pack (show pid)]
            testseg = substituteTemplate (segdecl segmentInfo) [testseg_name, segarg segmentInfo]
            codeLines = T.intercalate "\n  " (reverse (groupedCodes M.! pid))
        in eolc commentSyntax <> "  ===== TEST CODE SEGMENT " <> pack (show pid) <> " =====\n\n"
           <> testseg <> segbegin segmentInfo <> "\n"
           <> "  " <> codeLines <> "\n"
           <> segend segmentInfo
        ) sortedPids
      
      footer = eolc commentSyntax <> "  ===============================================\n\n"
      
  in header <> defSection <> "\n\n" <> declSection <> "\n\n" <> testSections <> "\n" <> footer

-- ==============================
-- 6. MAIN LOOP (IO at boundaries)
-- ==============================

initialState :: ProcessingState
initialState = ProcessingState
  { inSTRUCT = False
  , inSEQ = False
  , inName = ""
  , seqForSEQ = ""
  , defCode = []
  , declCode = []
  , testCodes = []
  , procIds = []
  , currentId = 0
  , switchNo = 0
  }

processInputStream :: Config -> IO ProcessingState
processInputStream config = go initialState
  where
    go state = do
      eof <- hIsEOF stdin
      if eof 
        then return state
        else do
          line <- T.getLine
          if "@@@" `T.isPrefixOf` line
            then let state' = processLine config state (T.drop 4 line)
                 in go state'
            else go state  -- Skip non-annotation lines

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  
  -- Read initial JSON config
  configLine <- T.getLine
  unless (T.toUpper configLine `T.isPrefixOf` "INIT JSON:") $ do
    T.putStrLn "ERROR: Expected INIT JSON: line"
    return ()
  
  let jsonText = T.drop 10 configLine  -- Remove "INIT JSON:"
  
  configResult <- parseConfigFromJson jsonText
  case configResult of
    Left err -> T.putStrLn $ "ERROR: " <> T.pack err
    Right config -> do
      T.putStrLn "RESPONSE:SUCCESS:Config loaded"
      
      -- Process annotations
      finalState <- processInputStream config
      
      -- Generate and output code
      let generatedCode = generateCode config finalState
      T.putStrLn generatedCode

-- Utility function (needed for encoding)
encodeUtf8 :: Text -> B.ByteString
encodeUtf8 = B.fromStrict . T.encodeUtf8