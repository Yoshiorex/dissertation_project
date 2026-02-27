{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module RefinementTests where


import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import GHC.Generics
import Data.Aeson as A
import Data.Aeson.Encode.Pretty as P
import Data.ByteString.Lazy.Char8 as B
import Data.Text as T
import Data.Char
import Data.List
import qualified Data.Map as Map
import Control.Exception (try, SomeException)

-- Import your main module
import Main (CommentCodes(..), DefaultCodes(..), RefDict(..), Flags(..), Config(..), 
             fieldToJSON, commentFieldToJSON, defaultCodesFieldToJSON, 
             parseConfig, setupLanguage, setupComments, setDefaults)

-- ============================================
-- Test Data Generators (QuickCheck)
-- ============================================

-- Generate random valid JSON keys
newtype JSONKey = JSONKey String deriving (Show, Eq)

instance Arbitrary JSONKey where
    arbitrary = do
        base <- elements ["language", "segnamepfx", "segarg", "segdecl", 
                         "segbegin", "segend", "name", "pending_DCL"]
        -- Sometimes add suffix
        suffix <- elements ["", "_DCL", "_PTR", "_FSCALAR"]
        return $ JSONKey (base ++ suffix)

-- Generate random field names
instance Arbitrary String where
    arbitrary = elements ["language", "segnamepfx", "segarg", "segdecl", 
                         "segbegin", "segend", "name", "runner", "worker"]

-- ============================================
-- Unit Tests for Field Mapping
-- ============================================

testFieldMapping :: Spec
testFieldMapping = describe "Field Mapping Functions" $ do
    describe "fieldToJSON" $ do
        it "maps lowercase to UPPERCASE for language fields" $ do
            fieldToJSON "language" `shouldBe` "LANGUAGE"
            fieldToJSON "segnamepfx" `shouldBe` "SEGNAMEPFX"
            fieldToJSON "segarg" `shouldBe` "SEGARG"
        
        it "handles special Haskell keyword conflicts" $ do
            fieldToJSON "initField" `shouldBe` "INIT"
            fieldToJSON "taskInit" `shouldBe` "TASK_INIT"
            fieldToJSON "waitField" `shouldBe` "WAIT"
        
        it "preserves snake_case fields unchanged" $ do
            fieldToJSON "pending_DCL" `shouldBe` "pending_DCL"
            fieldToJSON "semaphore_DCL" `shouldBe` "semaphore_DCL"
        
        it "handles mixed case fields correctly" $ do
            fieldToJSON "runner" `shouldBe` "Runner"
            fieldToJSON "worker" `shouldBe` "Worker"
            fieldToJSON "waitForSuspend" `shouldBe` "WaitForSuspend"
        
        it "returns field unchanged if no mapping exists" $ do
            fieldToJSON "unknownField" `shouldBe` "unknownField"
            fieldToJSON "custom_DCL" `shouldBe` "custom_DCL"
    
    describe "commentFieldToJSON" $ do
        it "maps comment field names correctly" $ do
            commentFieldToJSON "eolc" `shouldBe` "EOLC"
            commentFieldToJSON "cmtbegin" `shouldBe` "CMTBEGIN"
            commentFieldToJSON "cmtend" `shouldBe` "CMTEND"
            commentFieldToJSON "testlogger" `shouldBe` "TESTLOGGER"
    
    describe "defaultCodesFieldToJSON" $ do
        it "maps default code field names with 'default' prefix" $ do
            defaultCodesFieldToJSON "defaultsegnamepfx" `shouldBe` "SEGNAMEPFX"
            defaultCodesFieldToJSON "defaultsegarg" `shouldBe` "SEGARG"
            defaultCodesFieldToJSON "defaultsegdecl" `shouldBe` "SEGDECL"

-- ============================================
-- JSON Parsing Tests
-- ============================================

testJSONParsing :: Spec
testJSONParsing = describe "JSON Parsing" $ do
    let sampleJSON = "{\"ref_dict\": {\"LANGUAGE\": \"C\", \"SEGNAMEPFX\": \"TestSegment{}\", \
                     \\"SEGARG\": \"Context* ctx\", \"pending_DCL\": \"rtems_event_set {0}[TASK_MAX];\"}, \
                     \\"flags\": {\"outputLOG\": false, \"annoteComments\": true, \"outputSWITCH\": true}}"
    
    describe "parseConfig" $ do
        it "successfully parses valid JSON with INIT JSON: prefix" $ do
            let input = "INIT JSON:" ++ sampleJSON
            parseConfig input `shouldSatisfy` isJust
        
        it "returns Nothing for invalid JSON" $ do
            parseConfig "INIT JSON:{invalid json}" `shouldBe` Nothing
        
        it "returns Nothing for malformed input" $ do
            parseConfig "INIT JSON:" `shouldBe` Nothing
            parseConfig "" `shouldBe` Nothing
        
        it "correctly extracts all fields" $ do
            let input = "INIT JSON:" ++ sampleJSON
            case parseConfig input of
                Nothing -> expectationFailure "Should have parsed"
                Just config -> do
                    language (ref_dict config) `shouldBe` "C"
                    segnamepfx (ref_dict config) `shouldBe` "TestSegment{}"
                    segarg (ref_dict config) `shouldBe` "Context* ctx"
                    pending_DCL (ref_dict config) `shouldBe` "rtems_event_set {0}[TASK_MAX];"
                    outputLOG (flags config) `shouldBe` False
                    annoteComments (flags config) `shouldBe` True
                    outputSWITCH (flags config) `shouldBe` True
    
    describe "Config JSON instances" $ do
        it "round-trips JSON encoding/decoding" $ property $
            \lang nameField -> 
                let config = Config 
                        (RefDict lang "Test{}" "arg" "decl" "{" "}" nameField 
                         "pending" "semaphore" "sc" "prio" "create" "start" "delete"
                         "suspend" "isSuspend" "resume" "setPriority" "priority"
                         "taskID" "tasks" "init" "taskInit" "runner" "worker"
                         "signal" "wait" "task_create" "task_start" "task_delete"
                         "task_suspend" "task_isSuspended" "task_resume"
                         "task_setPriority" "createRC" "startRC" "delRC"
                         "suspendRC" "isSuspendRC" "resumeRC" "setPriorityRC"
                         "oldPrio" "waitForSuspend" "tooMany" "ready" "zombie"
                         "eventWait" "timeWait" "otherWait" "suspend" "wakeup"
                         "lowerPriority" "equalPriority" "higherPriority"
                         "startLog" "checkPreemption" "checkNoPreemption"
                         "runnerScheduler" "otherScheduler" "setProcessor")
                        (Flags False True True)
                    encoded = A.encode config
                    decoded = A.decode encoded :: Maybe Config
                in decoded == Just config

-- ============================================
-- Python Compatibility Tests
-- ============================================

testPythonCompatibility :: Spec
testPythonCompatibility = describe "Python Compatibility Tests" $ do
    describe "setupComments function" $ do
        it "returns CommentCodes for valid language" $ do
            result <- try (setupComments "c") :: IO (Either SomeException CommentCodes)
            result `shouldSatisfy` isRight
        
        it "recursively falls back to 'c' for unknown language" $ do
            result <- try (setupComments "nonexistent") :: IO (Either SomeException CommentCodes)
            result `shouldSatisfy` isRight
        
        it "fails when C language file is missing" $ do
            -- This test assumes the C file exists, so we'll test the logic differently
            pendingWith "Need to mock filesystem for this test"
    
    describe "setDefaults function" $ do
        it "returns DefaultCodes for valid language" $ do
            result <- try (setDefaults "c") :: IO (Either SomeException DefaultCodes)
            result `shouldSatisfy` isRight
        
        it "recursively falls back to 'c' for unknown language" $ do
            result <- try (setDefaults "nonexistent") :: IO (Either SomeException DefaultCodes)
            result `shouldSatisfy` isRight
    
    describe "setupLanguage function" $ do
        let testRefDict = RefDict "c" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" 
                              "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" 
                              "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""
        
        it "returns both CommentCodes and DefaultCodes" $ do
            (comments, defaults) <- setupLanguage testRefDict
            eolc comments `shouldSatisfy` (not . T.null)
            defaultsegnamepfx defaults `shouldSatisfy` (not . T.null)

-- ============================================
-- Property-Based Tests
-- ============================================

testProperties :: Spec
testProperties = describe "Property-Based Tests" $ do
    describe "Field mapping properties" $ do
        prop "fieldToJSON never returns empty string" $
            \fieldName -> not (null (fieldToJSON fieldName))
        
        prop "fieldToJSON preserves length for snake_case fields" $
            \fieldName -> 
                let result = fieldToJSON fieldName
                in if '_' `elem` fieldName && not (fieldName `elem` specialFields)
                   then length result >= length fieldName
                   else property True
          where specialFields = ["initField", "taskInit", "waitField"]
        
        prop "commentFieldToJSON converts to UPPERCASE" $
            \fieldName -> 
                let result = commentFieldToJSON fieldName
                in all isUpper result || result == fieldName
    
    describe "JSON parsing properties" $ do
        prop "parseConfig handles long inputs" $
            \jsonStr -> 
                let input = "INIT JSON:" ++ jsonStr
                in parseConfig input `seq` True  -- Shouldn't crash
        
        prop "Config serialization round-trip" $
            \config -> 
                let encoded = A.encode (config :: Config)
                    decoded = A.decode encoded
                in decoded == Just config

-- ============================================
-- Integration Tests (Comparing with Python)
-- ============================================

testPythonEquivalence :: Spec
testPythonEquivalence = describe "Python-Haskell Equivalence Tests" $ do
    describe "JSON parsing equivalence" $ do
        it "parses the same JSON as Python" $ do
            let jsonStr = "{\"ref_dict\": {\"LANGUAGE\": \"C\", \"SEGNAMEPFX\": \"TestSegment{}\"}, \
                          \\"flags\": {\"outputLOG\": false}}"
                input = "INIT JSON:" ++ jsonStr
            
            -- Haskell result
            let haskellResult = parseConfig input
            
            -- Expected Python-like behavior
            haskellResult `shouldSatisfy` isJust
            case haskellResult of
                Just config -> do
                    language (ref_dict config) `shouldBe` "C"
                    segnamepfx (ref_dict config) `shouldBe` "TestSegment{}"
                    outputLOG (flags config) `shouldBe` False
                Nothing -> expectationFailure "Should have parsed"
    
    describe "Field mapping equivalence with Python" $ do
        -- Test cases from Python's expected behavior
        let testCases = 
                [ ("language", "LANGUAGE")
                , ("segnamepfx", "SEGNAMEPFX")
                , ("pending_DCL", "pending_DCL")  -- Python keeps snake_case
                , ("semaphore_DCL", "semaphore_DCL")
                , ("runner", "Runner")  -- Python has mixed case
                , ("worker", "Worker")
                ]
        
        forM_ testCases $ \(haskellField, expectedJsonKey) ->
            it ("maps " ++ haskellField ++ " to " ++ expectedJsonKey) $
                fieldToJSON haskellField `shouldBe` expectedJsonKey

-- ============================================
-- Error Handling Tests
-- ============================================

testErrorHandling :: Spec
testErrorHandling = describe "Error Handling" $ do
    describe "File operations" $ do
        it "setupComments handles missing file gracefully" $ do
            pendingWith "Need filesystem mocking"
        
        it "setDefaults handles missing file gracefully" $ do
            pendingWith "Need filesystem mocking"
    
    describe "JSON error handling" $ do
        it "parseConfig doesn't crash on malformed JSON" $ do
            parseConfig "INIT JSON:{invalid}" `shouldBe` Nothing
            parseConfig "INIT JSON:{\"missing\": \"keys\"}" `shouldBe` Nothing
        
        it "handles missing required fields gracefully" $ do
            let jsonMissingFlags = "{\"ref_dict\": {\"LANGUAGE\": \"C\"}}"
                input = "INIT JSON:" ++ jsonMissingFlags
            parseConfig input `shouldBe` Nothing

-- ============================================
-- Main Test Runner
-- ============================================

main :: IO ()
main = hspec $ do
    testFieldMapping
    testJSONParsing
    testPythonCompatibility
    testProperties
    testPythonEquivalence
    testErrorHandling