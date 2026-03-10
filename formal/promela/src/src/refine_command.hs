{-# LANGUAGE OverloadedStrings #-}
module Main where

-- This executable is the Haskell implementation of the SPIN-line refiner.
-- It runs as a long-lived subprocess and receives text commands from Python
-- (`combined_haskell_bridge.py`). Every command updates an in-memory
-- `RuntimeState` and replies with a line-based SUCCESS/FAILED response.
--
-- High-level lifecycle:
-- 1) Python sends `INIT JSON:{...}` with language map + flags.
-- 2) Python sends many `COMMAND:refineSPINLine:[...]` calls.
-- 3) Python sends `COMMAND:testBody` to receive rendered C body text.

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
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getExecutablePath)
import System.IO (hIsEOF, hSetBuffering, BufferMode(LineBuffering), stdin, stdout)

-- `RefDict` holds YAML refinement entries such as:
-- "SIGNAL" -> "Wakeup( semaphore[{0}] );"
-- "SEGDECL" -> "static void {}( {} )"
--
-- We keep this as a simple association list to stay close to the original
-- Python representation and lookup behavior.
type RefDict = [(Text, Text)]

-- Runtime flags copied from Python constructor options.
data Flags = Flags
  { outputLOG :: Bool
  , annoteComments :: Bool
  , outputSWITCH :: Bool
  } deriving (Show)

-- Parsed content of `INIT JSON:{...}`.
data InputConfig = InputConfig
  { icRefDict :: RefDict
  , icFlags :: Flags
  } deriving (Show)

-- Complete mutable-style state threaded through pure functions.
-- `rt*` prefixes mirror prior Python instance fields.
data RuntimeState = RuntimeState
  { rtRefDict :: RefDict          -- Active template dictionary.
  , rtFlags :: Flags              -- Runtime toggle flags.
  , rtInSTRUCT :: Bool            -- True while between STRUCT ... END.
  , rtInSEQ :: Bool               -- True while between SEQ ... END.
  , rtSeqForSEQ :: Text           -- Accumulated SEQ scalar payload.
  , rtInName :: Text              -- Current STRUCT/SEQ symbolic name.
  , rtTestName :: Text            -- Reserved test-name field.
  , rtDefCode :: [Text]           -- Global #define / header text.
  , rtDeclCode :: [Text]          -- Global static declarations.
  , rtTestCodes :: [(Int, [Text])]-- Per-pid generated code fragments.
  , rtProcIds :: [Int]            -- Collected participant process IDs.
  , rtCurrentId :: Int            -- PID currently considered "running".
  , rtSwitchNo :: Int             -- Monotonic switch event counter.
  , rtEOLC :: Text                -- Language-specific end-of-line comment.
  , rtCMTBEGIN :: Text            -- Block-comment begin marker.
  , rtCMTEND :: Text              -- Block-comment end marker.
  , rtSegNamePfx :: Text          -- Template: segment function name.
  , rtSegArg :: Text              -- Template: segment argument list.
  , rtSegDecl :: Text             -- Template: segment declaration.
  , rtSegBegin :: Text            -- Template: segment body opener.
  , rtSegEnd :: Text              -- Template: segment body closer.
  } deriving (Show)

-- Top-level session wrapper. `Nothing` means INIT has not succeeded yet.
data Session = Session
  { ssRuntime :: Maybe RuntimeState
  }

-- JSON parsing for flags with defaults matching Python behavior.
instance A.FromJSON Flags where
  parseJSON = AT.withObject "Flags" $ \o ->
    Flags <$> o .:? "outputLOG" AT..!= False
          <*> o .:? "annoteComments" AT..!= True
          <*> o .:? "outputSWITCH" AT..!= True

-- JSON parsing for INIT payload.
-- `ref_dict` must be a JSON object and is converted to Text -> Text pairs.
instance A.FromJSON InputConfig where
  parseJSON = AT.withObject "InputConfig" $ \o -> do
    refVal <- o .: "ref_dict"
    flags <- o .:? "flags" AT..!= Flags False True True
    refDict <- case refVal of
      A.Object km -> pure $ keyMapToRefDict km
      _ -> fail "ref_dict must be an object"
    pure $ InputConfig refDict flags

-- Convert Aeson key map to our simple `RefDict` list format.
keyMapToRefDict :: KM.KeyMap A.Value -> RefDict
keyMapToRefDict km =
  [ (K.toText k, valueToText v) | (k, v) <- KM.toList km ]

-- Convert JSON values to plain text for template expansion.
-- Complex values are JSON-encoded into one compact text field.
valueToText :: A.Value -> Text
valueToText (A.String s) = s
valueToText (A.Number n) = T.pack (show n)
valueToText (A.Bool True) = "true"
valueToText (A.Bool False) = "false"
valueToText A.Null = ""
valueToText other = T.pack (BL.unpack (A.encode other))

-- Text `show` helper so template code stays `Text` end-to-end.
tshow :: Show a => a -> Text
tshow = T.pack . show

-- Keep behavior identical to Python `.split("\n")`.
splitLines :: Text -> [Text]
splitLines = T.splitOn "\n"

-- Replace first occurrence only (used for `{}` positional placeholders).
replaceFirst :: Text -> Text -> Text -> Text
replaceFirst needle repl txt =
  let (before, after) = T.breakOn needle txt
  in if T.null after
     then txt
     else before <> repl <> T.drop (T.length needle) after

-- Template formatter supporting both indexed (`{0}`) and sequential (`{}`):
-- 1) First replace `{0}`, `{1}`, ... with corresponding args.
-- 2) Then consume remaining bare `{}` left-to-right.
formatTemplate :: Text -> [Text] -> Text
formatTemplate template args =
  let indexed = L.foldl'
        (\acc (i, arg) -> T.replace ("{" <> tshow i <> "}") arg acc)
        template
        (zip [0 :: Int ..] args)
  in L.foldl' (\acc arg -> replaceFirst "{}" arg acc) indexed args

-- Discover `languages/<name>.yml` in common runtime locations.
-- We search relative to executable path and working directory so this works
-- both in-tree and from built artifacts.
findLanguageFile :: String -> IO (Maybe FilePath)
findLanguageFile lang = do
  exePath <- getExecutablePath
  cwd <- getCurrentDirectory
  let exeDir = dirname exePath
      fileName = L.map toLower lang ++ ".yml"
      candidates =
        [ exeDir ++ "/languages/" ++ fileName
        , cwd ++ "/languages/" ++ fileName
        , cwd ++ "/src/languages/" ++ fileName
        , cwd ++ "/src/src/languages/" ++ fileName
        , cwd ++ "/../languages/" ++ fileName
        , cwd ++ "/../src/languages/" ++ fileName
        ]
  firstExisting candidates
  where
    dirname p =
      case L.elemIndices '/' p of
        [] -> "."
        xs -> L.take (L.last xs) p
    firstExisting [] = pure Nothing
    firstExisting (p:ps) = do
      exists <- doesFileExist p
      if exists then pure (Just p) else firstExisting ps

-- Parse one language YAML file into a lookup map.
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

-- Try requested language; if unavailable and language is not already C, fall
-- back to C templates. This mirrors Python's tolerant startup behavior.
loadLanguageWithFallback :: String -> IO (Either String RefDict)
loadLanguageWithFallback lang = do
  res <- loadLanguageMap lang
  case res of
    Right m -> pure (Right m)
    Left _ ->
      if L.map toLower lang /= "c"
        then loadLanguageMap "c"
        else pure res

-- Read mandatory keys from language maps (missing keys are hard failures).
requireKey :: Text -> RefDict -> Either String Text
requireKey key m =
  case L.lookup key m of
    Just v -> Right v
    Nothing -> Left ("Missing required key in language YAML: " ++ T.unpack key)

-- Update comment delimiters from language map.
applyCommentsFromMap :: RefDict -> RuntimeState -> Either String RuntimeState
applyCommentsFromMap m rt = do
  eolc <- requireKey "EOLC" m
  cmtbegin <- requireKey "CMTBEGIN" m
  cmtend <- requireKey "CMTEND" m
  pure rt
    { rtEOLC = eolc
    , rtCMTBEGIN = cmtbegin
    , rtCMTEND = cmtend
    }

-- Fill optional segment rendering templates from language map.
applyDefaultsFromMap :: RefDict -> RuntimeState -> RuntimeState
applyDefaultsFromMap m rt =
  rt
    { rtSegNamePfx = fromMaybe (rtSegNamePfx rt) (L.lookup "SEGNAMEPFX" m)
    , rtSegArg = fromMaybe (rtSegArg rt) (L.lookup "SEGARG" m)
    , rtSegDecl = fromMaybe (rtSegDecl rt) (L.lookup "SEGDECL" m)
    , rtSegBegin = fromMaybe (rtSegBegin rt) (L.lookup "SEGBEGIN" m)
    , rtSegEnd = fromMaybe (rtSegEnd rt) (L.lookup "SEGEND" m)
    }

-- Re-apply segment rendering keys from `rtRefDict` itself.
-- This mirrors legacy behavior where the runtime map can override defaults.
setupSegmentCode :: RuntimeState -> RuntimeState
setupSegmentCode rt =
  rt
    { rtSegNamePfx = fromMaybe (rtSegNamePfx rt) (L.lookup "SEGNAMEPFX" (rtRefDict rt))
    , rtSegArg = fromMaybe (rtSegArg rt) (L.lookup "SEGARG" (rtRefDict rt))
    , rtSegDecl = fromMaybe (rtSegDecl rt) (L.lookup "SEGDECL" (rtRefDict rt))
    , rtSegBegin = fromMaybe (rtSegBegin rt) (L.lookup "SEGBEGIN" (rtRefDict rt))
    , rtSegEnd = fromMaybe (rtSegEnd rt) (L.lookup "SEGEND" (rtRefDict rt))
    }

-- Apply optional LANGUAGE override stored inside the current refinement map.
-- If LANGUAGE is absent, we keep current runtime values unchanged.
setupLanguageRuntime :: RuntimeState -> IO (Either String RuntimeState)
setupLanguageRuntime rt =
  case L.lookup "LANGUAGE" (rtRefDict rt) of
    Nothing -> pure (Right rt) -- Python does nothing when LANGUAGE key is absent
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
                  rt3 = setupSegmentCode rt2
              in pure (Right rt3)

-- Build initial runtime:
-- 1) start from hardcoded defaults
-- 2) apply baseline C language templates
-- 3) optionally apply LANGUAGE-specific override
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
          setupLanguageRuntime withDefaults
  where
    baseRuntime =
      RuntimeState
        { rtRefDict = refDict
        , rtFlags = flags
        , rtInSTRUCT = False
        , rtInSEQ = False
        , rtSeqForSEQ = ""
        , rtInName = ""
        , rtTestName = "Un-named Test"
        , rtDefCode = []
        , rtDeclCode = []
        , rtTestCodes = []
        , rtProcIds = []
        , rtCurrentId = 0
        , rtSwitchNo = 0
        , rtEOLC = ""
        , rtCMTBEGIN = ""
        , rtCMTEND = ""
        , rtSegNamePfx = "TestSegment{}"
        , rtSegArg = "Context* ctx"
        , rtSegDecl = "static void {}( {} )"
        , rtSegBegin = " {"
        , rtSegEnd = "}"
        }

-- Ref dictionary lookup helper.
lookupRef :: Text -> RuntimeState -> Maybe Text
lookupRef key rt = L.lookup key (rtRefDict rt)

-- Parse full-text PID (fails if trailing data remains).
parsePidText :: Text -> Maybe Int
parsePidText t =
  case TR.decimal t of
    Right (n, rest) | T.null rest -> Just n
    _ -> Nothing

-- PID parser with default 0 for malformed input.
pidFromText :: Text -> Int
pidFromText = fromMaybe 0 . parsePidText

-- Append one generated code chunk to `(pid, [lines])` list.
addCode :: Int -> [Text] -> RuntimeState -> RuntimeState
addCode pid lines0 rt =
  rt { rtTestCodes = rtTestCodes rt ++ [(pid, lines0)] }

-- Utility for template entries that have two alternatives:
-- if numeric value is 0 choose `line0`, else choose `line1`.
addCodeInt :: Int -> Text -> Text -> Text -> RuntimeState -> RuntimeState
addCodeInt pid line0 line1 value rt
  | T.all isDigit value =
      if value == "0"
        then addCode pid [line0] rt
        else addCode pid [line1] rt
  | otherwise = rt

-- Insert PID if not yet present, preserving insertion order.
addUniquePid :: Int -> [Int] -> [Int]
addUniquePid pid pids =
  if pid `elem` pids then pids else pids ++ [pid]

-- Collect PID directly from tokenized SPIN line. This path is used when
-- Python streams full lines through `COMMAND:refineSPINLine`.
collectPIdsTokens :: [Text] -> RuntimeState -> RuntimeState
collectPIdsTokens ln rt =
  case ln of
    pidTxt:_ ->
      case parsePidText pidTxt of
        Just pid -> rt { rtProcIds = addUniquePid pid (rtProcIds rt) }
        Nothing -> rt
    _ -> rt

-- Inject synthetic scheduling code when control moves from one PID to another.
-- Output examples come from YAML keys:
-- - SUSPEND: previous PID yields
-- - WAKEUP : next PID resumes
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

-- Add trace comments/logs describing the raw SPIN source line.
-- This is controlled by `annoteComments`.
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

-- Main token-pattern refiner. Each case converts one SPIN instruction form
-- into C code snippets by looking up templates in `rtRefDict`.
--
-- `spinLine` convention:
-- - head token is usually pid
-- - second token is opcode (LOG/DECL/CALL/...)
-- - remaining tokens are opcode arguments
refineSPINLine :: [Text] -> RuntimeState -> RuntimeState
refineSPINLine spinLine rt =
  case spinLine of
    -- Direct log passthrough when outputLOG flag is enabled.
    pidTxt:"LOG":rest ->
      if outputLOG (rtFlags rt)
        then addCode (pidFromText pidTxt) ["T_log(T_NORMAL," <> T.unwords rest <> ");"] rt
        else rt

    -- Test name/header initialization.
    pidTxt:"NAME":_name:[] ->
      case lookupRef "NAME" rt of
        Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " CANNOT REFINE 'NAME'"] rt
        Just code -> addCode (pidFromText pidTxt) (splitLines code) rt

    -- Global test initialization.
    pidTxt:"INIT":[] ->
      case lookupRef "INIT" rt of
        Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " CANNOT REFINE 'INIT'"] rt
        Just code -> addCode (pidFromText pidTxt) (splitLines code) rt

    -- Task alias opcodes dispatch directly by taskName key.
    pidTxt:"TASK":taskName:[] ->
      case lookupRef taskName rt of
        Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " CANNOT REFINE TASK " <> taskName] rt
        Just code -> addCode (pidFromText pidTxt) (splitLines code) rt

    -- Semaphore/event synchronization helpers.
    pidTxt:"SIGNAL":value:[] ->
      case lookupRef "SIGNAL" rt of
        Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " CANNOT REFINE SIGNAL " <> value] rt
        Just code -> addCode (pidFromText pidTxt) (splitLines (formatTemplate code [value])) rt

    pidTxt:"WAIT":value:[] ->
      case lookupRef "WAIT" rt of
        Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " CANNOT REFINE WAIT " <> value] rt
        Just code -> addCode (pidFromText pidTxt) (splitLines (formatTemplate code [value])) rt

    -- Macro definitions always go to global definition section.
    _pidTxt:"DEF":name:value:[] ->
      rt { rtDefCode = rtDefCode rt ++ ["#define " <> name <> " " <> value] }

    -- Scalar declaration with optional initializer. Key lookup uses `<name>_DCL`.
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

    -- Array declaration. Template receives `[name, value]`.
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

    -- Pointer assignment has two modes:
    -- - normal var pointer (`*_PTR`)
    -- - struct field pointer (`*_FPTR`) when inside STRUCT block
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

    -- General function-style call, with template argument expansion.
    pidTxt:"CALL":name:args ->
      case lookupRef name rt of
        Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " CALL: no refinement entry for '" <> name <> "'"] rt
        Just code ->
          if length args > 6
            then addCode (pidFromText pidTxt) [rtEOLC rt <> " CALL: can't handle > 6 arguments"] rt
            else
              let callCode = if null args then code else formatTemplate code args
              in addCode (pidFromText pidTxt) (splitLines callCode) rt

    -- Begin STRUCT field-context mode (changes SCALAR/PTR behavior).
    _pidTxt:"STRUCT":name:[] ->
      rt { rtInSTRUCT = True, rtInName = name }

    -- Begin sequence-capture mode. `_` scalars are concatenated until END.
    _pidTxt:"SEQ":name:[] ->
      rt { rtInSEQ = True, rtSeqForSEQ = "", rtInName = name }

    -- End STRUCT or SEQ mode and emit any accumulated SEQ template result.
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

    -- Special SEQ accumulation token: SCALAR _ <value>.
    _pidTxt:"SCALAR":"_":value:[] ->
      rt { rtSeqForSEQ = rtSeqForSEQ rt <> " " <> value }

    -- SCALAR with single value:
    -- - outside STRUCT: use `<name>`
    -- - inside STRUCT : use `<name>_FSCALAR` with `rtInName` as field owner
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

    -- SCALAR with index + value variant.
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

    -- Thread state checks/refinements.
    pidTxt:"STATE":tid:stateName:[] ->
      case lookupRef stateName rt of
        Nothing -> addCode (pidFromText pidTxt) [rtEOLC rt <> " STATE: no refinement entry for '" <> stateName <> "'"] rt
        Just code -> addCode (pidFromText pidTxt) [formatTemplate code [tid]] rt

    -- Catch-all diagnostics for currently unsupported opcodes.
    pidTxt:hd:_rest ->
      addCode (pidFromText pidTxt) [rtEOLC rt <> "  DON'T KNOW HOW TO REFINE: " <> pidTxt <> " '" <> hd] rt

    [pidTxt] ->
      addCode (pidFromText pidTxt) [rtEOLC rt <> "  DON'T KNOW HOW TO REFINE: " <> pidTxt] rt

    _ -> rt

-- Full line-processing pipeline:
-- 1) collect PID
-- 2) optionally insert switch code
-- 3) optionally add comments/log mirrors
-- 4) apply opcode refinement
refineSPINLineMain :: [Text] -> RuntimeState -> RuntimeState
refineSPINLineMain ln rt0 =
  let rt1 = collectPIdsTokens ln rt0
      rt2 = if outputSWITCH (rtFlags rt1) then switchIfRequired ln rt1 else rt1
      rt3 = if annoteComments (rtFlags rt2) then logSPINLine ln rt2 else rt2
  in refineSPINLine ln rt3

-- Render final output sections in deterministic order:
-- definitions, declarations, then grouped test segments by PID.
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

-- Parse explicit collectPIds command used by bridge:
-- `COMMAND:collectPIds:<pid>`
parseCollectPidCommand :: Text -> Maybe Int
parseCollectPidCommand line =
  let prefix = "COMMAND:collectPIds:"
  in if prefix `T.isPrefixOf` line
       then
         case TR.decimal (T.drop (T.length prefix) line) of
           Right (pid, rest) | T.null rest -> Just pid
           _ -> Nothing
       else Nothing

-- Parse refine command payload:
-- `COMMAND:refineSPINLine:<json-array>`
-- where payload decodes to `[Text]`.
parseRefineLineCommand :: Text -> Maybe [Text]
parseRefineLineCommand line =
  let prefix = "COMMAND:refineSPINLine:"
  in if prefix `T.isPrefixOf` line
       then
         let payload = T.drop (T.length prefix) line
         in A.decode (BL.pack (T.unpack payload))
       else Nothing

-- Handle startup INIT and build initial runtime state.
handleInitCommand :: Session -> Text -> IO Session
handleInitCommand session line = do
  let payload = T.drop (T.length ("INIT JSON:" :: Text)) line
  case A.decode (BL.pack (T.unpack payload)) :: Maybe InputConfig of
    Nothing -> do
      putStrLn "RESPONSE:FAILED:Failed to parse JSON as Config"
      pure session
    Just config -> do
      initRes <- initRuntime config
      case initRes of
        Left err -> do
          putStrLn ("RESPONSE:FAILED:" ++ err)
          pure session
        Right rt -> do
          putStrLn "RESPONSE:SUCCESS:INIT JSON received"
          pure session { ssRuntime = Just rt }

-- Re-apply LANGUAGE-specific templates after initialization.
handleSetupLanguageCommand :: Session -> IO Session
handleSetupLanguageCommand session =
  case ssRuntime session of
    Nothing -> do
      putStrLn "RESPONSE:FAILED:No config initialized"
      pure session
    Just rt -> do
      res <- setupLanguageRuntime rt
      case res of
        Left err -> do
          putStrLn ("RESPONSE:FAILED:" ++ err)
          pure session
        Right rt' -> do
          putStrLn "RESPONSE:SUCCESS:setupLanguage worked"
          pure session { ssRuntime = Just rt' }

-- Add one PID to runtime state and return the full PID set.
handleCollectPIdsCommand :: Session -> Text -> IO Session
handleCollectPIdsCommand session line =
  case ssRuntime session of
    Nothing -> do
      putStrLn "RESPONSE:FAILED:No config initialized"
      pure session
    Just rt ->
      case parseCollectPidCommand line of
        Nothing -> do
          putStrLn "RESPONSE:FAILED:Invalid collectPIds command"
          pure session
        Just pid -> do
          let rt' = rt { rtProcIds = addUniquePid pid (rtProcIds rt) }
          putStrLn ("RESPONSE:SUCCESS:collectPIds:" ++ formatPidSet (rtProcIds rt'))
          pure session { ssRuntime = Just rt' }

-- Refine one SPIN token line and update state.
handleRefineSPINLineCommand :: Session -> Text -> IO Session
handleRefineSPINLineCommand session line =
  case ssRuntime session of
    Nothing -> do
      putStrLn "RESPONSE:FAILED:No config initialized"
      pure session
    Just rt ->
      case parseRefineLineCommand line of
        Nothing -> do
          putStrLn "RESPONSE:FAILED:Invalid refineSPINLine command"
          pure session
        Just tokens -> do
          let rt' = refineSPINLineMain tokens rt
          putStrLn "RESPONSE:SUCCESS:refineSPINLine"
          pure session { ssRuntime = Just rt' }

-- Stream final generated code body using explicit begin/end markers so Python
-- can read multiline content safely.
handleTestBodyCommand :: Session -> IO Session
handleTestBodyCommand session =
  case ssRuntime session of
    Nothing -> do
      putStrLn "RESPONSE:FAILED:No config initialized"
      pure session
    Just rt -> do
      putStrLn "RESPONSE:SUCCESS:testBody:BEGIN"
      putStrLn (T.unpack (renderTestBody rt))
      putStrLn "RESPONSE:SUCCESS:testBody:END"
      pure session

-- Command router for one input line.
handleLine :: Session -> Text -> IO Session
handleLine session line
  | "INIT JSON:" `T.isPrefixOf` line = handleInitCommand session line
  | line == "COMMAND:setupLanguage" = handleSetupLanguageCommand session
  | "COMMAND:collectPIds:" `T.isPrefixOf` line = handleCollectPIdsCommand session line
  | "COMMAND:refineSPINLine:" `T.isPrefixOf` line = handleRefineSPINLineCommand session line
  | line == "COMMAND:testBody" = handleTestBodyCommand session
  | otherwise = do
      putStrLn ("DEBUG: Unknown command: " ++ T.unpack (T.take 80 line))
      putStrLn "RESPONSE:FAILED:Unknown command"
      pure session

-- Tail-recursive REPL loop (process exits on stdin EOF).
commandLoop :: Session -> IO ()
commandLoop session = do
  eof <- hIsEOF stdin
  if eof
    then putStrLn "DEBUG: EOF reached, exiting"
    else do
      line <- getLine
      next <- handleLine session (T.pack line)
      commandLoop next

-- Entrypoint for interactive mode used by Python bridge.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  cwd <- getCurrentDirectory
  putStrLn ("Haskell: Started in directory: " ++ cwd)
  putStrLn "Haskell: Started - WITH EOF HANDLING"
  commandLoop (Session Nothing)
