{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Data.List
import Data.Aeson as A
import Data.Aeson.Encode.Pretty as P
import Data.Aeson.Types (Options(..), defaultOptions)
import System.IO as S
import Data.ByteString.Lazy.Char8 as B
import Data.Text as T
import Data.Char
import qualified Data.ByteString as BS
import Control.Monad (void)
import Data.Yaml
import System.Directory
import System.Exit

-- Define fields in lowercase (Haskell convention)
data CommentCodes = CommentCodes
  { eolc :: Text
  , cmtbegin :: Text
  , cmtend :: Text
  } deriving (Show, Generic)

data DefaultCodes = RefinementCodes -- Keeping user's constructor name
  { defaultsegnamepfx :: Text
  , defaultsegarg :: Text
  , defaultsegdecl :: Text
  , defaultsegbegin :: Text
  , defaultsegend :: Text
  } deriving (Show, Generic)

-- NEW: SegmentCodes type to hold resolved segment configuration
data SegmentCodes = SegmentCodes
  { scNamePfx :: Text
  , scArg :: Text
  , scDecl :: Text
  , scBegin :: Text
  , scEnd :: Text
  } deriving (Show, Generic)

data RefDict = RefDict
  { language :: Text
  , segnamepfx :: Text
  , segarg :: Text
  , segdecl :: Text
  , segbegin :: Text
  , segend :: Text
  , name :: Text
  , pending_DCL :: Text
  , semaphore_DCL :: Text
  , sc_DCL :: Text
  , prio_DCL :: Text
  , createRC_DCL :: Text
  , startRC_DCL :: Text
  , deleteRC_DCL :: Text
  , suspendRC_DCL :: Text
  , isSuspendRC_DCL :: Text
  , resumeRC_DCL :: Text
  , setPriorityRC_DCL :: Text
  , priority_DCL :: Text
  , taskID_DCL :: Text
  , tasks_DCL :: Text
  , initField :: Text
  , taskInit :: Text
  , runner :: Text
  , worker :: Text
  , signal :: Text
  , waitField :: Text
  , task_create :: Text
  , task_start :: Text
  , task_delete :: Text
  , task_suspend :: Text
  , task_isSuspended :: Text
  , task_resume :: Text
  , task_setPriority :: Text
  , createRC :: Text
  , startRC :: Text
  , delRC :: Text
  , suspendRC :: Text
  , isSuspendRC :: Text
  , resumeRC :: Text
  , setPriorityRC :: Text
  , oldPrio :: Text
  , waitForSuspend :: Text
  , tooMany :: Text
  , ready :: Text
  , zombie :: Text
  , eventWait :: Text
  , timeWait :: Text
  , otherWait :: Text
  , suspend :: Text
  , wakeup :: Text
  , lowerPriority :: Text
  , equalPriority :: Text
  , higherPriority :: Text
  , startLog :: Text
  , checkPreemption :: Text
  , checkNoPreemption :: Text
  , runnerScheduler :: Text
  , otherScheduler :: Text
  , setProcessor :: Text
  } deriving (Show, Generic)

data Flags = Flags
    { outputLOG :: Bool
    , annoteComments :: Bool
    , outputSWITCH :: Bool
    } deriving (Show, Generic)

data Config = Config
    { ref_dict :: RefDict
    , flags :: Flags
    } deriving (Show, Generic)

data ProcessState = ProcessState
  { inSTRUCT :: Bool
  , inSEQ :: Bool
  , seqForSEQ :: Text
  , inName :: Text
  , testName :: Text
  , defCode :: [Text]
  , declCode :: [Text]
  , testCodes :: [(Int, [Text])]  -- list of (pid, code lines)
  , procIds :: Int
  , currentId :: Int
  , switchNo :: Int
  , commentCodes :: CommentCodes
  , segmentCodes :: SegmentCodes
  , refDict :: RefDict
  , p_flags :: Flags
  }

fieldToJSON :: String -> String
fieldToJSON fieldName =
    case fieldName of
        "language" -> "LANGUAGE"
        "segnamepfx" -> "SEGNAMEPFX"
        "segarg" -> "SEGARG"
        "segdecl" -> "SEGDECL"
        "segbegin" -> "SEGBEGIN"
        "segend" -> "SEGEND"
        "name" -> "NAME"
        "initField" -> "INIT"
        "taskInit" -> "TASK_INIT"
        "waitField" -> "WAIT"
        "runner" -> "Runner"
        "worker" -> "Worker"
        "signal" -> "SIGNAL"
        "waitForSuspend" -> "WaitForSuspend"
        "tooMany" -> "TooMany"
        "ready" -> "Ready"
        "zombie" -> "Zombie"
        "eventWait" -> "EventWait"
        "timeWait" -> "TimeWait"
        "otherWait" -> "OtherWait"
        "suspend" -> "SUSPEND"
        "wakeup" -> "WAKEUP"
        "lowerPriority" -> "LowerPriority"
        "equalPriority" -> "EqualPriority"
        "higherPriority" -> "HigherPriority"
        "startLog" -> "StartLog"
        "checkPreemption" -> "CheckPreemption"
        "checkNoPreemption" -> "CheckNoPreemption"
        "runnerScheduler" -> "RunnerScheduler"
        "otherScheduler" -> "OtherScheduler"
        "setProcessor" -> "SetProcessor"
        _ -> fieldName

instance FromJSON RefDict where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = fieldToJSON
        }

instance ToJSON RefDict where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = fieldToJSON
        }

commentFieldToJSON :: String -> String
commentFieldToJSON fieldName =
    case fieldName of
        "eolc" -> "EOLC"
        "cmtbegin" -> "CMTBEGIN"
        "cmtend" -> "CMTEND"
        _ -> fieldName

instance FromJSON CommentCodes where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = commentFieldToJSON
        }

instance ToJSON CommentCodes where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = commentFieldToJSON
        }

defaultCodesFieldToJSON :: String -> String
defaultCodesFieldToJSON fieldName =
    case fieldName of
      "defaultsegnamepfx" -> "SEGNAMEPFX"
      "defaultsegarg" -> "SEGARG"
      "defaultsegdecl" -> "SEGDECL"
      "defaultsegbegin" -> "SEGBEGIN"
      "defaultsegend" -> "SEGEND"
      _ -> fieldName

instance FromJSON DefaultCodes where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = defaultCodesFieldToJSON
        , omitNothingFields = True
        }

instance ToJSON DefaultCodes where
    toJSON :: DefaultCodes -> Value
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = defaultCodesFieldToJSON
        , omitNothingFields = True
        }

instance FromJSON Flags
instance ToJSON Flags
instance FromJSON Main.Config
instance ToJSON Main.Config



setupComments :: String -> IO CommentCodes
setupComments language = do
  S.putStrLn ("Set LANGUAGE to " ++ language ++ "(comments)\n")
  let filename = "../../src/src/languages/" ++ Data.List.map Data.Char.toLower language ++ ".yml"
  
  exists <- doesFileExist filename
  
  if exists 
    then do
      result <- decodeFileEither filename
      case result of
        Left err -> do
          print err
          exitWith (ExitFailure 1)
        Right parsed -> do
          if T.null (eolc parsed) || T.null (cmtbegin parsed) || T.null (cmtend parsed)
            then do
              S.putStrLn $ "Missing required comment keys in " ++ filename
              exitWith (ExitFailure 1)
            else do
              return parsed
    else if Data.List.map Data.Char.toLower language /= "c"
      then do
        S.putStrLn $ "Unknown LANGUAGE " ++ language ++ " set to C\n"
        setupComments "c"
      else do
        S.putStrLn $ "Ensure language file for " ++ language ++ 
                     " is present before generating tests (comments)"
        S.putStrLn $ "File " ++ filename ++ " not found\n"
        exitWith (ExitFailure 1)

setDefaults :: String -> IO DefaultCodes 
setDefaults language = do
    S.putStrLn $ "Set LANGUAGE to " ++ language ++ " (non-comment defaults)\n"
    let filename = "../../src/src/languages/" ++ Data.List.map Data.Char.toLower language ++ ".yml"
    
    exists <- doesFileExist filename
    
    if exists 
        then do
            result <- decodeFileEither filename :: IO (Either ParseException DefaultCodes)
            case result of
                Left err -> do
                    print err
                    exitWith (ExitFailure 1)
                Right parsed -> do
                    return parsed
        else if Data.List.map Data.Char.toLower language /= "c"
            then do
                S.putStrLn $ "Unknown LANGUAGE " ++ language ++ ", set to C\n"
                setDefaults "c"
            else do
                S.putStrLn $ "Ensure language file for " ++ language ++ 
                           " is present before generating tests (non-comment defaults)\n"
                S.putStrLn $ "File " ++ filename ++ " not found\n"
                exitWith (ExitFailure 1)

-- NEW: setupSegmentCode function
-- Logic: Uses RefDict values if present (Haskell types are strict, so they are present).
-- If we needed to support missing keys (falling back to defaults), RefDict fields would need to be Maybe.
setupSegmentCode :: RefDict -> DefaultCodes -> IO SegmentCodes
setupSegmentCode refDict defaults = do
    -- In Python: if key in ref_dict_keys -> use ref_dict, else use default
    -- Here we extract from RefDict.
    
    let pfx = segnamepfx refDict
        arg = segarg refDict
        decl = segdecl refDict
        begin = segbegin refDict
        end = segend refDict

    -- Debug logging
    S.putStrLn $ "SEGNAMEPFX is '" ++ T.unpack pfx ++ "'"
    S.putStrLn $ "SEGARG is '" ++ T.unpack arg ++ "'"
    S.putStrLn $ "SEGDECL is '" ++ T.unpack decl ++ "'"
    S.putStrLn $ "SEGBEGIN is '" ++ T.unpack begin ++ "'"
    S.putStrLn $ "SEGEND is '" ++ T.unpack end ++ "'"

    return $ SegmentCodes pfx arg decl begin end

-- UPDATED: setupLanguage now returns SegmentCodes as well
setupLanguage :: RefDict -> IO (CommentCodes, DefaultCodes, SegmentCodes)
setupLanguage refdict = do
    let langText = language refdict
        lowerLang = T.unpack $ T.map Data.Char.toLower langText
    
    commentCodes <- setupComments lowerLang
    defaultCodes <- setDefaults lowerLang
    
    -- Call setupSegmentCode
    segmentCodes <- setupSegmentCode refdict defaultCodes
    
    return (commentCodes, defaultCodes, segmentCodes)

parseConfig :: String -> Maybe Main.Config
parseConfig line =
    let jsonStr = Data.List.drop 10 line
    in A.decode (B.pack jsonStr)



collectPIds procIds spinline = 
    if not(Data.List.null spinline)
      then procIds ++ Data.List.head(read spinline)
    else procIds

addCode testCodes id codelines =
    testCodes ++ [(read id, codelines)]

addCodeInt pid f_codelines0 f_codelines1 value =

    if value == "0" && Data.List.all isDigit value
            then addCode pid f_codelines0
          else addCode pid f_codelines1
  
    

main :: IO ()
main = do
  --runTests
  hSetBuffering stdout LineBuffering
  currentDir <- getCurrentDirectory
  S.putStrLn $ "Haskell: Started in directory: " ++ currentDir
  S.putStrLn "Haskell: Started - WITH EOF HANDLING"

  let loop = do
        eof <- hIsEOF stdin
        if eof
          then S.putStrLn "DEBUG: EOF reached, exiting"
          else do
            line <- getLine

            if "INIT JSON:" `Data.List.isPrefixOf` line
              then do
                S.putStrLn "DEBUG: Processing INIT JSON command"

                case parseConfig line of
                    Nothing -> do
                        S.putStrLn "Failed to parse JSON as Config"
                        case (A.decode (B.pack (Data.List.drop 10 line)) :: Maybe A.Value) of
                            Nothing -> S.putStrLn "Failed to parse JSON at all"
                            Just jsonValue -> do
                                S.putStrLn "=== Generic JSON (not Config) ==="
                                B.putStrLn $ A.encode jsonValue
                        loop

                    Just config -> do
                        S.putStrLn "=== Successfully parsed as Config ==="
                        S.putStrLn $ "Language: " ++ T.unpack (language (ref_dict config))
                        
                        -- UPDATED: Capture the new return values from setupLanguage
                        (cmtCodes, defCodes, segCodes) <- setupLanguage (ref_dict config)
                        
                        -- Example of accessing the resolved SegmentCodes
                        S.putStrLn $ "Resolved Segment Prefix: " ++ T.unpack (scNamePfx segCodes)
                        
                        S.putStrLn $ "Segment Prefix (Raw RefDict): " ++ T.unpack (segnamepfx (ref_dict config))
                        S.putStrLn $ "outputLOG: " ++ Prelude.show (outputLOG (flags config))

                        S.putStrLn "RESPONSE:SUCCESS:INIT JSON received"
                        loop

              else if line == "COMMAND:setupLanguage"
                then do
                  S.putStrLn "RESPONSE:SUCCESS:setupLanguage worked"
                  loop
                else do
                  S.putStrLn $ "DEBUG: Unknown command: " ++ Data.List.take 50 line
                  S.putStrLn "RESPONSE:FAILED:Unknown command"
                  loop

  loop

--- ============================================
-- TESTS AT THE BOTTOM OF THE FILE
-- ============================================

-- Helper to run tests

runTests :: IO ()
runTests = do
  -- Use $ to ensure the string calculation happens before putStrLn
  S.putStrLn $ "\n" ++ Data.List.replicate 60 '='
  S.putStrLn "RUNNING TESTS"
  S.putStrLn $ Data.List.replicate 60 '=' ++ "\n"
  
  -- Test 1: Field mapping
  S.putStrLn "Test 1: Field mapping"
  S.putStrLn $ "  fieldToJSON 'language' = '" ++ fieldToJSON "language" ++ "'"
  S.putStrLn $ "  Expected: 'LANGUAGE'"
  S.putStrLn $ "  " ++ if fieldToJSON "language" == "LANGUAGE" then "✓ PASS" else "✗ FAIL"
  
  S.putStrLn $ "  fieldToJSON 'segnamepfx' = '" ++ fieldToJSON "segnamepfx" ++ "'"
  S.putStrLn $ "  Expected: 'SEGNAMEPFX'"
  S.putStrLn $ "  " ++ if fieldToJSON "segnamepfx" == "SEGNAMEPFX" then "✓ PASS" else "✗ FAIL"
  
  S.putStrLn $ "  fieldToJSON 'runner' = '" ++ fieldToJSON "runner" ++ "'"
  S.putStrLn $ "  Expected: 'Runner'"
  S.putStrLn $ "  " ++ if fieldToJSON "runner" == "Runner" then "✓ PASS" else "✗ FAIL"
  
  -- Test 2: JSON parsing
  let fullTestJSON = Data.List.concat
        [ "{\"ref_dict\": {"
        , "\"LANGUAGE\": \"C\","
        , "\"SEGNAMEPFX\": \"Seg\","
        , "\"SEGARG\": \"Arg\","
        , "\"SEGDECL\": \"Decl\","
        , "\"SEGBEGIN\": \"Begin\","
        , "\"SEGEND\": \"End\","
        , "\"NAME\": \"Name\","
        , "\"pending_DCL\": \"pdcl\","
        , "\"semaphore_DCL\": \"sdcl\","
        , "\"sc_DCL\": \"scdcl\","
        , "\"prio_DCL\": \"pdcl\","
        , "\"createRC_DCL\": \"crdcl\","
        , "\"startRC_DCL\": \"srdcl\","
        , "\"deleteRC_DCL\": \"drdcl\","
        , "\"suspendRC_DCL\": \"sudcl\","
        , "\"isSuspendRC_DCL\": \"isdcl\","
        , "\"resumeRC_DCL\": \"rmdcl\","
        , "\"setPriorityRC_DCL\": \"spdcl\","
        , "\"priority_DCL\": \"prdcl\","
        , "\"taskID_DCL\": \"tidcl\","
        , "\"tasks_DCL\": \"tdcl\","
        , "\"INIT\": \"init\","
        , "\"TASK_INIT\": \"taskInit\","
        , "\"Runner\": \"runner\","
        , "\"Worker\": \"worker\","
        , "\"SIGNAL\": \"signal\","
        , "\"WAIT\": \"wait\","
        , "\"task_create\": \"create\","
        , "\"task_start\": \"start\","
        , "\"task_delete\": \"delete\","
        , "\"task_suspend\": \"suspend\","
        , "\"task_isSuspended\": \"isSuspended\","
        , "\"task_resume\": \"resume\","
        , "\"task_setPriority\": \"setPrio\","
        , "\"createRC\": \"crc\","
        , "\"startRC\": \"src\","
        , "\"delRC\": \"drc\","
        , "\"suspendRC\": \"surc\","
        , "\"isSuspendRC\": \"isrc\","
        , "\"resumeRC\": \"rerc\","
        , "\"setPriorityRC\": \"sprc\","
        , "\"oldPrio\": \"oldPrio\","
        , "\"WaitForSuspend\": \"wfs\","
        , "\"TooMany\": \"tm\","
        , "\"Ready\": \"rdy\","
        , "\"Zombie\": \"zb\","
        , "\"EventWait\": \"ew\","
        , "\"TimeWait\": \"tw\","
        , "\"OtherWait\": \"ow\","
        , "\"SUSPEND\": \"susp\","
        , "\"WAKEUP\": \"wake\","
        , "\"LowerPriority\": \"lp\","
        , "\"EqualPriority\": \"ep\","
        , "\"HigherPriority\": \"hp\","
        , "\"StartLog\": \"sl\","
        , "\"CheckPreemption\": \"cp\","
        , "\"CheckNoPreemption\": \"cnp\","
        , "\"RunnerScheduler\": \"rs\","
        , "\"OtherScheduler\": \"os\","
        , "\"SetProcessor\": \"sp\""
        , "}, \"flags\": {"
        , "\"outputLOG\": true,"
        , "\"annoteComments\": false,"
        , "\"outputSWITCH\": true"
        , "}}"
        ]
      testInput = "INIT JSON:" ++ fullTestJSON
  
  S.putStrLn "\nTest 2: JSON parsing"
  case parseConfig testInput of
    Nothing -> S.putStrLn "  ✗ FAIL: Could not parse JSON"
    Just config -> do
      S.putStrLn "  ✓ PASS: JSON parsed successfully"
      S.putStrLn $ "  Language = " ++ T.unpack (language (ref_dict config))
      S.putStrLn $ "  outputLOG = " ++ Prelude.show (outputLOG (flags config))
  
  -- Test 3: commentFieldToJSON
  S.putStrLn "\nTest 3: Comment field mapping"
  S.putStrLn $ "  commentFieldToJSON 'eolc' = '" ++ commentFieldToJSON "eolc" ++ "'"
  S.putStrLn $ "  Expected: 'EOLC'"
  S.putStrLn $ "  " ++ if commentFieldToJSON "eolc" == "EOLC" then "✓ PASS" else "✗ FAIL"
  
  S.putStrLn $ "\n" ++ Data.List.replicate 60 '='
  S.putStrLn "TESTS COMPLETE"
  S.putStrLn $ Data.List.replicate 60 '=' ++ "\n"

-- Quick property tests
quickPropertyTests :: IO ()
quickPropertyTests = do
  S.putStrLn "\nQuick Property Tests:"
  
  -- Property: fieldToJSON never returns empty string
  let testFields = ["language", "segnamepfx", "runner", "unknown", ""]
      allNonEmpty = Data.List.all (not . Data.List.null . fieldToJSON) testFields
  S.putStrLn $ "  Property: fieldToJSON never returns empty: "
           ++ if allNonEmpty then "✓ PASS" else "✗ FAIL"
  
  -- Property: commentFieldToJSON returns uppercase
  let commentFields = ["eolc", "cmtbegin", "cmtend"]
      allUppercase = Data.List.all (\f -> Data.List.all isUpper (commentFieldToJSON f)) commentFields
  S.putStrLn $ "  Property: commentFieldToJSON returns uppercase: "
           ++ if allUppercase then "✓ PASS" else "✗ FAIL"

-- Individual test functions you can call from GHCi
test_fieldMapping :: IO ()
test_fieldMapping = do
  S.putStrLn "Testing field mapping..."
  let testCases = 
        [ ("language", "LANGUAGE")
        , ("segnamepfx", "SEGNAMEPFX")
        , ("segarg", "SEGARG")
        , ("runner", "Runner")
        , ("worker", "Worker")
        , ("pending_DCL", "pending_DCL")
        , ("unknown", "unknown")
        ]
  
  mapM_ (\(input, expected) -> do
    let result = fieldToJSON input
    S.putStrLn $ "  " ++ input ++ " -> " ++ result
    if result == expected
      then S.putStrLn "    ✓ PASS"
      else S.putStrLn $ "    ✗ FAIL (expected " ++ expected ++ ")"
    ) testCases
test_jsonParsing :: IO ()
test_jsonParsing = do
  S.putStrLn "Testing JSON parsing..."
  
  -- Fix: Use Data.List.concat for String lists, instead of T.concat
  let fullTestJSON = Data.List.concat
        [ "{\"ref_dict\": {"
        , "\"LANGUAGE\": \"C\","
        , "\"SEGNAMEPFX\": \"Seg\","
        , "\"SEGARG\": \"Arg\","
        , "\"SEGDECL\": \"Decl\","
        , "\"SEGBEGIN\": \"Begin\","
        , "\"SEGEND\": \"End\","
        , "\"NAME\": \"Name\","
        , "\"pending_DCL\": \"pdcl\","
        , "\"semaphore_DCL\": \"sdcl\","
        , "\"sc_DCL\": \"scdcl\","
        , "\"prio_DCL\": \"pdcl\","
        , "\"createRC_DCL\": \"crdcl\","
        , "\"startRC_DCL\": \"srdcl\","
        , "\"deleteRC_DCL\": \"drdcl\","
        , "\"suspendRC_DCL\": \"sudcl\","
        , "\"isSuspendRC_DCL\": \"isdcl\","
        , "\"resumeRC_DCL\": \"rmdcl\","
        , "\"setPriorityRC_DCL\": \"spdcl\","
        , "\"priority_DCL\": \"prdcl\","
        , "\"taskID_DCL\": \"tidcl\","
        , "\"tasks_DCL\": \"tdcl\","
        , "\"INIT\": \"init\","
        , "\"TASK_INIT\": \"taskInit\","
        , "\"Runner\": \"runner\","
        , "\"Worker\": \"worker\","
        , "\"SIGNAL\": \"signal\","
        , "\"WAIT\": \"wait\","
        , "\"task_create\": \"create\","
        , "\"task_start\": \"start\","
        , "\"task_delete\": \"delete\","
        , "\"task_suspend\": \"suspend\","
        , "\"task_isSuspended\": \"isSuspended\","
        , "\"task_resume\": \"resume\","
        , "\"task_setPriority\": \"setPrio\","
        , "\"createRC\": \"crc\","
        , "\"startRC\": \"src\","
        , "\"delRC\": \"drc\","
        , "\"suspendRC\": \"surc\","
        , "\"isSuspendRC\": \"isrc\","
        , "\"resumeRC\": \"rerc\","
        , "\"setPriorityRC\": \"sprc\","
        , "\"oldPrio\": \"oldPrio\","
        , "\"WaitForSuspend\": \"wfs\","
        , "\"TooMany\": \"tm\","
        , "\"Ready\": \"rdy\","
        , "\"Zombie\": \"zb\","
        , "\"EventWait\": \"ew\","
        , "\"TimeWait\": \"tw\","
        , "\"OtherWait\": \"ow\","
        , "\"SUSPEND\": \"susp\","
        , "\"WAKEUP\": \"wake\","
        , "\"LowerPriority\": \"lp\","
        , "\"EqualPriority\": \"ep\","
        , "\"HigherPriority\": \"hp\","
        , "\"StartLog\": \"sl\","
        , "\"CheckPreemption\": \"cp\","
        , "\"CheckNoPreemption\": \"cnp\","
        , "\"RunnerScheduler\": \"rs\","
        , "\"OtherScheduler\": \"os\","
        , "\"SetProcessor\": \"sp\""
        , "}, \"flags\": {"
        , "\"outputLOG\": true,"
        , "\"annoteComments\": false,"
        , "\"outputSWITCH\": true"
        , "}}"
        ]

  case parseConfig ("INIT JSON:" ++ fullTestJSON) of
    Nothing -> S.putStrLn "  ✗ FAIL: Could not parse valid JSON (Check if all fields are present)"
    Just config -> do
      S.putStrLn "  ✓ PASS: Parsed valid JSON"
      S.putStrLn $ "    Language = " ++ T.unpack (language (ref_dict config))
      S.putStrLn $ "    Segment prefix = " ++ T.unpack (segnamepfx (ref_dict config))
      S.putStrLn $ "    outputLOG = " ++ Prelude.show (outputLOG (flags config))
  
  -- Test case 2: Invalid JSON
  case parseConfig "INIT JSON:{invalid json}" of
    Nothing -> S.putStrLn "  ✓ PASS: Correctly rejected invalid JSON"
    Just _ -> S.putStrLn "  ✗ FAIL: Should have rejected invalid JSON"