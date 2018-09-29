module SpecUtils
    ( emptyCodeTemplate
    , evalEmp
    , normalizeLunaCode
    , runEmp
    , runTests
    , testCase
    , testCaseWithMarkers
    , withChannels
    , xitWithReason
    , module X
    ) where


import           Control.Concurrent.MVar       (newEmptyMVar)
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TChan  (newTChan)
import           Data.Char                     (isSpace)
import           Data.List                     (dropWhileEnd)
import qualified Data.Text                     as Text
import           Data.Reflection               (Given, give)
import qualified Empire.Commands.Graph         as Graph
import           Empire.Commands.Library       (createLibrary)
import           Control.Exception             (bracket)
import           Empire.Data.Graph             (CommandState (CommandState), defaultPMState)
import           Empire.Empire                 (CommunicationEnv (CommunicationEnv), Empire, Env
                                               , runEmpire)
import           Empire.Prelude
import qualified Empire.Data.Graph             as Graph
import           LunaStudio.Data.GraphLocation (GraphLocation (GraphLocation))
import qualified LunaStudio.Data.Node          as Node
import           Test.Hspec                    (Arg, Example, Expectation, Spec, SpecWith
                                               , around, before_, describe, it, parallel, pendingWith, shouldBe)
import           Text.RawString.QQ             (r)

import           SpecUtils.Graph               as X
    

withChannels :: (CommunicationEnv -> IO a) -> IO a
withChannels = bracket createChannels (const $ pure ()) where
    createChannels = CommunicationEnv 
        <$> atomically newTChan <*> newEmptyMVar <*> newEmptyMVar

runEmp :: CommunicationEnv -> (Given GraphLocation => Empire a) -> IO (a, CommandState Env)
runEmp env act = defaultPMState >>= \pm ->
    runEmpire env (CommandState pm def) $ do
        let testFile = "/TestFile"
            topGl    = GraphLocation testFile def
        void $ createLibrary (Just testFile) testFile
        Graph.loadCode topGl "def main:\n    None"
        [node] <- Graph.getNodes topGl
        give (topGl |>= (node ^. Node.nodeId)) act

evalEmp :: CommunicationEnv -> (Given GraphLocation => Empire a) -> IO a
evalEmp env act = fst <$> runEmp env act


runTests :: String -> SpecWith CommunicationEnv -> Spec
runTests = around withChannels . parallel .: describe

xitWithReason :: (HasCallStack, Example a) 
    => String -> String -> a -> SpecWith (Arg a)
xitWithReason label reason action 
    = before_ (pendingWith reason) $ it label action


emptyCodeTemplate :: Text
emptyCodeTemplate = [r|
import Std.Base

def main:
    None
|]

normalizeLunaCode :: Text -> Text
normalizeLunaCode str = Text.intercalate "\n" $ Text.drop minWs <$> allLines where
    trimTrailingSpaces = Text.dropWhileEnd isSpace
    trimEmptyLines     = dropWhileEnd Text.null . dropWhile Text.null
    indentLength       = Text.length . Text.takeWhile isSpace
    allLines = trimEmptyLines $ trimTrailingSpaces <$> Text.lines str
    minWs    = minimum $ indentLength <$> filter (not . Text.null) allLines

codeCheck :: Text -> (Text -> Expectation)
codeCheck expectedCode = \resultCode -> 
    Text.strip resultCode `shouldBe` normalizeLunaCode expectedCode

testCase
    :: Text
    -> Text
    -> (GraphLocation -> Empire a)
    -> CommunicationEnv
    -> Expectation
testCase initialCode expectedCode action env = let
        filePath = "/TestPath"
        topGl    = GraphLocation filePath def
        execute  = do
            createLibrary Nothing filePath
            Graph.loadCode topGl $ normalizeLunaCode initialCode
            let mainNodeName = "main"
                withMain mainNodeId = do
                    let gl = topGl |>= mainNodeId
                    mockNodesLayout gl
                    pure gl
            mainNodeId <- findNodeIdByName topGl mainNodeName
            gl <- maybe (pure topGl) withMain mainNodeId
            action gl
            Graph.getCode gl
    in evalEmp env execute >>= codeCheck expectedCode
        
-- This function is copy paste of testCase and is meant to be removed soon, when markers are removed from Luna
testCaseWithMarkers
    :: Text
    -> Text
    -> (GraphLocation -> Empire a)
    -> CommunicationEnv
    -> Expectation
testCaseWithMarkers initialCode expectedCode action env = let
        filePath = "/TestPath"
        topGl    = GraphLocation filePath def
        execute  = do
            createLibrary Nothing filePath
            Graph.loadCode topGl $ normalizeLunaCode initialCode
            let mainNodeName = "main"
                withMain mainNodeId = do
                    let gl = topGl |>= mainNodeId
                    mockNodesLayout gl
                    pure gl
            mainNodeId <- findNodeIdByName topGl mainNodeName
            gl <- maybe (pure topGl) withMain mainNodeId
            action gl
            Graph.withGraph gl (use Graph.code)
    in evalEmp env execute >>= codeCheck expectedCode