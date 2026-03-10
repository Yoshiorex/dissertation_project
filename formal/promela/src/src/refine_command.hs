{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson as A
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AT
import Data.Char (isDigit, toLower)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Read as TR
import qualified Data.Yaml as Y
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.Exit (exitFailure)
import System.IO (hIsEOF, hSetBuffering, BufferMode(LineBuffering), stdin, stdout)

type RefDict = [(Text, Text)]

data Flags = Flags
  { outputLOG :: Bool
  , annoteComments :: Bool
  , outputSWITCH :: Bool
  } deriving (Show)

data InputConfig = InputConfig
  { icRefDict :: RefDict
  , icFlags :: Flags
  } deriving (Show)

data RuntimeState = RuntimeState
  { rtRefDict :: RefDict
  , rtFlags :: Flags
  , rtInSTRUCT :: Bool
  , rtInSEQ :: Bool
  , rtSeqForSEQ :: Text
  , rtInName :: Text
  , rtDefCode :: [Text]
  , rtDeclCode :: [Text]
  , rtTestCodes :: [(Int, [Text])]
  , rtProcIds :: [Int]
  , rtCurrentId :: Int
  , rtSwitchNo :: Int
  , rtEOLC :: Text
  , rtSegNamePfx :: Text
  , rtSegArg :: Text
  , rtSegDecl :: Text
  , rtSegBegin :: Text
  , rtSegEnd :: Text
  } deriving (Show)

instance A.FromJSON Flags where
  parseJSON = AT.withObject "Flags" $ \o ->
    Flags <$> o .:? "outputLOG" AT..!= False
          <*> o .:? "annoteComments" AT..!= True
          <*> o .:? "outputSWITCH" AT..!= True

instance A.FromJSON InputConfig where
  parseJSON = AT.withObject "InputConfig" $ \o -> do
    refVal <- o .: "ref_dict"
    flags <- o .:? "flags" AT..!= Flags False True True
    refDict <- case refVal of
      A.Object km -> pure $ keyMapToRefDict km
      _ -> fail "ref_dict must be an object"
    pure $ InputConfig refDict flags

keyMapToRefDict :: KM.KeyMap A.Value -> RefDict
keyMapToRefDict km =
  [ (K.toText k, valueToText v) | (k, v) <- KM.toList km ]

valueToText :: A.Value -> Text
valueToText (A.String s) = s
valueToText (A.Number n) = T.pack (show n)
valueToText (A.Bool True) = "true"
valueToText (A.Bool False) = "false"
valueToText A.Null = ""
valueToText other = T.pack (BL.unpack (A.encode other))

tshow :: Show a => a -> Text
tshow = T.pack . show

splitLines :: Text -> [Text]
splitLines = T.splitOn "\n"

replaceFirst :: Text -> Text -> Text -> Text
replaceFirst needle repl txt =
  let (before, after) = T.breakOn needle txt
  in if T.null after
     then txt
     else before <> repl <> T.drop (T.length needle) after

formatTemplate :: Text -> [Text] -> Text
formatTemplate template args =
  let indexed = L.foldl'
        (\acc (i, arg) -> T.replace ("{" <> tshow i <> "}") arg acc)
        template
        (zip [0 :: Int ..] args)
  in L.foldl' (\acc arg -> replaceFirst "{}" arg acc) indexed args

findLanguageFile :: String -> IO (Maybe FilePath)
findLanguageFile lang = do
  exePath <- getExecutablePath
  let dir = dirname exePath
      fileName = L.map toLower lang ++ ".yml"
      path = dir ++ "/languages/" ++ fileName
  exists <- doesFileExist path
  pure (if exists then Just path else Nothing)
  where
    dirname p =
      case L.elemIndices '/' p of
        [] -> "."
        xs -> L.take (L.last xs) p

loadLanguageMap :: String -> IO (Either String RefDict)
loadLanguageMap lang = do
  mfile <- findLanguageFile lang
  case mfile of
    Nothing -> pure $ Left ("Language file not found for " ++ lang)
    Just path -> do
      parsed <- Y.decodeFileEither path :: IO (Either Y.ParseException A.Value)
      case parsed of
        Left err -> pure $ Left (show err)
        Right (A.Object obj) -> pure $ Right (keyMapToRefDict obj)
        Right _ -> pure $ Left ("Language file does not contain an object: " ++ path)

loadLanguageWithFallback :: String -> IO (Either String RefDict)
loadLanguageWithFallback lang = do
  res <- loadLanguageMap lang
  case res of
    Right m -> pure (Right m)
    Left _ ->
      if L.map toLower lang /= "c"
        then loadLanguageMap "c"
        else pure res

requireKey :: Text -> RefDict -> Either String Text
requireKey key m =
  case L.lookup key m of
    Just v -> Right v
    Nothing -> Left ("Missing required key in language YAML: " ++ T.unpack key)

applyCommentsFromMap :: RefDict -> RuntimeState -> Either String RuntimeState
applyCommentsFromMap m rt = do
  eolc <- requireKey "EOLC" m
  pure rt { rtEOLC = eolc }

applyDefaultsFromMap :: RefDict -> RuntimeState -> RuntimeState
applyDefaultsFromMap m rt =
  rt
    { rtSegNamePfx = fromMaybe (rtSegNamePfx rt) (L.lookup "SEGNAMEPFX" m)
    , rtSegArg = fromMaybe (rtSegArg rt) (L.lookup "SEGARG" m)
    , rtSegDecl = fromMaybe (rtSegDecl rt) (L.lookup "SEGDECL" m)
    , rtSegBegin = fromMaybe (rtSegBegin rt) (L.lookup "SEGBEGIN" m)
    , rtSegEnd = fromMaybe (rtSegEnd rt) (L.lookup "SEGEND" m)
    }

setupLanguageRuntime :: RuntimeState -> IO (Either String RuntimeState)
setupLanguageRuntime rt =
  case L.lookup "LANGUAGE" (rtRefDict rt) of
    Nothing -> pure (Right rt)
    Just langText -> do
      let lang = L.map toLower (T.unpack langText)
      langMapRes <- loadLanguageWithFallback lang
      case langMapRes of
        Left err -> pure (Left err)
        Right langMap ->
          case applyCommentsFromMap langMap rt of
            Left err -> pure (Left err)
            Right rt1 ->
              let rt2 = applyDefaultsFromMap langMap rt1
                  rt3 = applyDefaultsFromMap (rtRefDict rt) rt2
              in pure (Right rt3)

initRuntime :: InputConfig -> IO (Either String RuntimeState)
initRuntime (InputConfig refDict flags) = do
  cMapRes <- loadLanguageMap "c"
  case cMapRes of
    Left err -> pure (Left err)
    Right cMap ->
      case applyCommentsFromMap cMap baseRuntime of
        Left err -> pure (Left err)
        Right withComments -> do
          let withDefaults = applyDefaultsFromMap cMap withComments
              withOverrides = applyDefaultsFromMap refDict withDefaults
          setupLanguageRuntime withOverrides
  where
    baseRuntime =
      RuntimeState
        { rtRefDict = refDict
        , rtFlags = flags
        , rtInSTRUCT = False
        , rtInSEQ = False
        , rtSeqForSEQ = ""
        , rtInName = ""
        , rtDefCode = []
        , rtDeclCode = []
        , rtTestCodes = []
        , rtProcIds = []
        , rtCurrentId = 0
        , rtSwitchNo = 0
        , rtEOLC = ""
        , rtSegNamePfx = "TestSegment{}"
        , rtSegArg = "Context* ctx"
        , rtSegDecl = "static void {}( {} )"
        , rtSegBegin = " {"
        , rtSegEnd = "}"
        }

lookupRef :: Text -> RuntimeState -> Maybe Text
lookupRef key rt = L.lookup key (rtRefDict rt)

parsePidText :: Text -> Maybe Int
parsePidText t =
  case TR.decimal t of
    Right (n, rest) | T.null rest -> Just n
    _ -> Nothing

pidFromText :: Text -> Int
pidFromText = fromMaybe 0 . parsePidText

addCode :: Int -> [Text] -> RuntimeState -> RuntimeState
addCode pid lines0 rt =
  rt { rtTestCodes = rtTestCodes rt ++ [(pid, lines0)] }

addCodeInt :: Int -> Text -> Text -> Text -> RuntimeState -> RuntimeState
addCodeInt pid line0 line1 value rt
  | T.all isDigit value =
      if value == "0"
        then addCode pid [line0] rt
        else addCode pid [line1] rt
  | otherwise = rt

addUniquePid :: Int -> [Int] -> [Int]
addUniquePid pid pids =
  if pid `elem` pids then pids else pids ++ [pid]

collectPIdsTokens :: [Text] -> RuntimeState -> RuntimeState
collectPIdsTokens ln rt =
  case ln of
    pidTxt:_ ->
      case parsePidText pidTxt of
        Just pid -> rt { rtProcIds = addUniquePid pid (rtProcIds rt) }
        Nothing -> rt
    _ -> rt

switchIfRequired :: [Text] -> RuntimeState -> RuntimeState
switchIfRequired ln rt =
  case ln of
    pidTxt:_ ->
      case parsePidText pidTxt of
        Nothing -> rt
        Just pid ->
          if pid == rtCurrentId rt
            then rt
            else
              let nextSwitch = rtSwitchNo rt + 1
                  suspendArgs = [tshow nextSwitch, tshow (rtCurrentId rt), tshow pid]
                  wakeupArgs = [tshow nextSwitch, tshow pid, tshow (rtCurrentId rt)]
                  rt1 =
                    case lookupRef "SUSPEND" rt of
                      Nothing -> addCode pid [rtEOLC rt <> " SUSPEND: no refinement entry found"] rt
                      Just code -> addCode (rtCurrentId rt) [formatTemplate code suspendArgs] rt
                  rt2 =
                    case lookupRef "WAKEUP" rt of
                      Nothing -> addCode pid [rtEOLC rt <> " WAKEUP: no refinement entry found"] rt1
                      Just code -> addCode pid [formatTemplate code wakeupArgs] rt1
              in rt2 { rtCurrentId = pid, rtSwitchNo = nextSwitch }
    _ -> rt

logSPINLine :: [Text] -> RuntimeState -> RuntimeState
logSPINLine ln rt =
  if length ln > 1
    then
      let pid = pidFromText (head ln)
          msg = rtEOLC rt <> " @@@ " <> T.unwords ln
          kw = ln !! 1
      in case kw of
          "NAME" -> rt { rtDefCode = rtDefCode rt ++ [msg] }
          "DEF" -> rt { rtDefCode = rtDefCode rt ++ [msg] }
          "DECL" ->
            if pid == 0
              then rt { rtDeclCode = rtDeclCode rt ++ [msg] }
              else addCode pid [msg] rt
          "DCLARRAY" ->
            if pid == 0
              then rt { rtDeclCode = rtDeclCode rt ++ [msg] }
              else addCode pid [msg] rt
          "LOG" -> rt
          _ -> addCode pid ["T_log(T_NORMAL,\"@@@ " <> T.unwords ln <> "\");"] rt
    else rt

refineSPINLine :: [Text] -> RuntimeState -> RuntimeState
refineSPINLine spinLine rt =
  case spinLine of
    pidTxt:"LOG":rest ->
      if outputLOG (rtFlags rt)
        then addCode (pidFromText pidTxt) ["T_log(T_NORMAL," <> T.unwords rest <> ");"] rt
        else rt

    pidTxt:"NAME":_name:[] ->
      case lookupRef "NAME" rt of
        Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " CANNOT REFINE 'NAME'"] rt
        Just code -> addCode (pidFromText pidTxt) (splitLines code) rt

    pidTxt:"INIT":[] ->
      case lookupRef "INIT" rt of
        Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " CANNOT REFINE 'INIT'"] rt
        Just code -> addCode (pidFromText pidTxt) (splitLines code) rt

    pidTxt:"TASK":taskName:[] ->
      case lookupRef taskName rt of
        Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " CANNOT REFINE TASK " <> taskName] rt
        Just code -> addCode (pidFromText pidTxt) (splitLines code) rt

    pidTxt:"SIGNAL":value:[] ->
      case lookupRef "SIGNAL" rt of
        Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " CANNOT REFINE SIGNAL " <> value] rt
        Just code -> addCode (pidFromText pidTxt) (splitLines (formatTemplate code [value])) rt

    pidTxt:"WAIT":value:[] ->
      case lookupRef "WAIT" rt of
        Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " CANNOT REFINE WAIT " <> value] rt
        Just code -> addCode (pidFromText pidTxt) (splitLines (formatTemplate code [value])) rt

    _pidTxt:"DEF":name:value:[] ->
      rt { rtDefCode = rtDefCode rt ++ ["#define " <> name <> " " <> value] }

    pidTxt:"DECL":_typ:name:rest ->
      let key = name <> "_DCL"
          baseDecl =
            case lookupRef key rt of
              Nothing -> rtEOLC rt <> " CANNOT REFINE Decl " <> key
              Just code -> code <> " " <> name
          fullDecl =
            case rest of
              v:_ -> baseDecl <> " = " <> v <> ";"
              [] -> baseDecl <> ";"
      in if pidFromText pidTxt == 0
           then rt { rtDeclCode = rtDeclCode rt ++ ["static " <> fullDecl] }
           else addCode (pidFromText pidTxt) [fullDecl] rt

    pidTxt:"DCLARRAY":_typ:name:value:[] ->
      let key = name <> "_DCL"
          dclLines =
            case lookupRef key rt of
              Nothing -> [rtEOLC rt <> " DCLARRAY: no refinement entry for '" <> key <> "'"]
              Just code ->
                let out = formatTemplate code [name, value]
                in splitLines (if pidFromText pidTxt == 0 then "static " <> out else out)
      in if pidFromText pidTxt == 0
           then rt { rtDeclCode = rtDeclCode rt ++ dclLines }
           else addCode (pidFromText pidTxt) dclLines rt

    pidTxt:"PTR":name:value:[] ->
      if not (rtInSTRUCT rt)
        then
          let pname = name <> "_PTR"
          in case lookupRef pname rt of
              Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " PTR: no refinement entry for '" <> pname <> "'"] rt
              Just code ->
                let pcode = splitLines code
                    line0 = if null pcode then "" else head pcode
                    line1 = if length pcode > 1 then formatTemplate (pcode !! 1) [value] else ""
                in addCodeInt (pidFromText pidTxt) line0 line1 value rt
        else
          let pname = name <> "_FPTR"
          in case lookupRef pname rt of
              Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " PTR(field): no refinement for '" <> pname <> "'"] rt
              Just code ->
                let pcode = splitLines code
                    line0 = if null pcode then "" else head pcode
                    line1 = if length pcode > 1 then formatTemplate (pcode !! 1) [rtInName rt, value] else ""
                in addCodeInt (pidFromText pidTxt) line0 line1 value rt

    pidTxt:"CALL":name:args ->
      case lookupRef name rt of
        Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " CALL: no refinement entry for '" <> name <> "'"] rt
        Just code ->
          if length args > 6
            then addCode (pidFromText pidTxt) [rtEOLC rt <> " CALL: can't handle > 6 arguments"] rt
            else
              let callCode = if null args then code else formatTemplate code args
              in addCode (pidFromText pidTxt) (splitLines callCode) rt

    _pidTxt:"STRUCT":name:[] ->
      rt { rtInSTRUCT = True, rtInName = name }

    _pidTxt:"SEQ":name:[] ->
      rt { rtInSEQ = True, rtSeqForSEQ = "", rtInName = name }

    pidTxt:"END":name:[] ->
      let pid = pidFromText pidTxt
          rt1 = if rtInSTRUCT rt then rt { rtInSTRUCT = False } else rt
          rt2 =
            if rtInSEQ rt1
              then
                let seqName = name <> "_SEQ"
                in case lookupRef seqName rt1 of
                    Nothing -> addCode pid ["SEQ END: no refinement for "] rt1
                    Just code ->
                      L.foldl'
                        (\acc line0 -> addCode pid [formatTemplate line0 [rtSeqForSEQ rt1]] acc)
                        rt1
                        (splitLines code)
              else rt1
          rt3 =
            if rtInSEQ rt1
              then rt2 { rtInSEQ = False, rtSeqForSEQ = "" }
              else rt2
      in rt3 { rtInName = "" }

    _pidTxt:"SCALAR":"_":value:[] ->
      rt { rtSeqForSEQ = rtSeqForSEQ rt <> " " <> value }

    pidTxt:"SCALAR":name:value:[] ->
      if not (rtInSTRUCT rt)
        then
          case lookupRef name rt of
            Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " SCALAR: no refinement entry for '" <> name <> "'"] rt
            Just code -> addCode (pidFromText pidTxt) (splitLines (formatTemplate code [value])) rt
        else
          let field = name <> "_FSCALAR"
          in case lookupRef field rt of
              Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " SCALAR(field): no refinement entry for '" <> field <> "'"] rt
              Just code -> addCode (pidFromText pidTxt) [formatTemplate code [rtInName rt, value]] rt

    pidTxt:"SCALAR":name:index:value:[] ->
      if not (rtInSTRUCT rt)
        then
          case lookupRef name rt of
            Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " SCALAR-3: no refinement entry for '" <> name <> "'"] rt
            Just code -> addCode (pidFromText pidTxt) (splitLines (formatTemplate code [index, value])) rt
        else
          let field = name <> "_FSCALAR"
          in case lookupRef field rt of
              Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " SCALAR(field): no refinement entry for '" <> field <> "'"] rt
              Just code -> addCode (pidFromText pidTxt) [formatTemplate code [rtInName rt, value]] rt

    pidTxt:"STATE":tid:stateName:[] ->
      case lookupRef stateName rt of
        Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " STATE: no refinement entry for '" <> stateName <> "'"] rt
        Just code -> addCode (pidFromText pidTxt) [formatTemplate code [tid]] rt

    pidTxt:hd:_rest ->
      addCode (pidFromText pidTxt) [rtEOLC rt <> "  DON'T KNOW HOW TO REFINE: " <> pidTxt <> " '" <> hd] rt

    [pidTxt] ->
      addCode (pidFromText pidTxt) [rtEOLC rt <> "  DON'T KNOW HOW TO REFINE: " <> pidTxt] rt

    _ -> rt

refineSPINLineMain :: [Text] -> RuntimeState -> RuntimeState
refineSPINLineMain ln rt0 =
  let rt1 = collectPIdsTokens ln rt0
      rt2 = if outputSWITCH (rtFlags rt1) then switchIfRequired ln rt1 else rt1
      rt3 = if annoteComments (rtFlags rt2) then logSPINLine ln rt2 else rt2
  in refineSPINLine ln rt3

renderTestBody :: RuntimeState -> Text
renderTestBody rt =
  let header = "\n" <> rtEOLC rt <> "  ===============================================\n\n"
      defSection = T.concat [line <> "\n" | line <- rtDefCode rt]
      declSection = T.concat [line <> "\n" | line <- rtDeclCode rt]
      sortedCodes = L.sortOn fst (rtTestCodes rt)
      groups = L.groupBy (\a b -> fst a == fst b) sortedCodes
      segmentText = T.concat (map (renderSegment rt) groups)
      footer = "\n" <> rtEOLC rt <> "  ===============================================\n\n"
  in header <> defSection <> declSection <> segmentText <> footer
  where
    renderSegment _ [] = ""
    renderSegment st grp =
      let sno = fst (head grp)
          segName = formatTemplate (rtSegNamePfx st) [tshow sno]
          segDecl = formatTemplate (rtSegDecl st) [segName, rtSegArg st]
          bodyLines =
            T.concat
              [ T.concat ["  ", line, "\n"]
              | (_pid, lines0) <- grp
              , line <- lines0
              ]
      in "\n"
         <> rtEOLC st <> "  ===== TEST CODE SEGMENT " <> tshow sno <> " =====\n\n"
         <> segDecl <> rtSegBegin st <> "\n"
         <> bodyLines
         <> rtSegEnd st <> "\n"

formatPidSet :: [Int] -> String
formatPidSet pids =
  L.intercalate "," (map show (L.sort pids))

commandLoop :: RuntimeState -> IO ()
commandLoop rt = do
  eof <- hIsEOF stdin
  if eof
    then pure ()
    else do
      line <- getLine
      rt' <- handleLine rt (T.pack line)
      commandLoop rt'

handleLine :: RuntimeState -> Text -> IO RuntimeState
handleLine rt line
  | line == "COMMAND:setupLanguage" = do
      res <- setupLanguageRuntime rt
      case res of
        Left err -> putStrLn ("RESPONSE:FAILED:" ++ err) >> pure rt
        Right rt' -> putStrLn "RESPONSE:SUCCESS:setupLanguage" >> pure rt'
  | "COMMAND:collectPIds:" `T.isPrefixOf` line =
      let prefix = "COMMAND:collectPIds:"
          payload = T.drop (T.length prefix) line
      in case parsePidText payload of
          Nothing -> putStrLn "RESPONSE:FAILED:Invalid collectPIds" >> pure rt
          Just pid -> do
            let rt' = rt { rtProcIds = addUniquePid pid (rtProcIds rt) }
            putStrLn ("RESPONSE:SUCCESS:collectPIds:" ++ formatPidSet (rtProcIds rt'))
            pure rt'
  | "COMMAND:refineSPINLine:" `T.isPrefixOf` line =
      let prefix = "COMMAND:refineSPINLine:"
          payload = T.drop (T.length prefix) line
      in case A.decode (BL.pack (T.unpack payload)) of
          Nothing -> putStrLn "RESPONSE:FAILED:Invalid refineSPINLine" >> pure rt
          Just tokens -> do
            let rt' = refineSPINLineMain tokens rt
            putStrLn "RESPONSE:SUCCESS:refineSPINLine"
            pure rt'
  | line == "COMMAND:testBody" = do
      putStrLn "RESPONSE:SUCCESS:testBody:BEGIN"
      putStrLn (T.unpack (renderTestBody rt))
      putStrLn "RESPONSE:SUCCESS:testBody:END"
      pure rt
  | otherwise = do
      putStrLn "RESPONSE:FAILED:Unknown command"
      pure rt

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  initLine <- getLine
  let payload = T.drop (T.length ("INIT JSON:" :: Text)) (T.pack initLine)
  case A.decode (BL.pack (T.unpack payload)) :: Maybe InputConfig of
    Nothing -> do
      putStrLn "RESPONSE:FAILED:Failed to parse JSON as Config"
      exitFailure
    Just config -> do
      initRes <- initRuntime config
      case initRes of
        Left err -> do
          putStrLn ("RESPONSE:FAILED:" ++ err)
          exitFailure
        Right rt -> do
          putStrLn "RESPONSE:SUCCESS:INIT JSON received"
          commandLoop rt
