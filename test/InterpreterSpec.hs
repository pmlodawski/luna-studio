---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module InterpreterSpec where

import Control.Monad.State hiding (mapM, mapM_)
import Test.Hspec
import Text.Show.Pretty

import qualified Common
import qualified Flowbox.Interpreter.Session.AST.Executor       as Executor
import qualified Flowbox.Interpreter.Session.AST.Traverse       as Traverse
import qualified Flowbox.Interpreter.Session.Data.CallDataPath  as CallDataPath
import           Flowbox.Interpreter.Session.Data.CallPoint     (CallPoint (CallPoint))
import           Flowbox.Interpreter.Session.Data.CallPointPath (CallPointPath)
import qualified Flowbox.Interpreter.Session.Env                as Env
import           Flowbox.Interpreter.Session.Session            (Session)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified SampleCodes


rootLogger :: Logger
rootLogger = getLogger "Flowbox"



getArgs :: CallPointPath -> Session [CallPointPath]
getArgs callPointPath = do
    mainPtr <- gets $ view Env.mainPtr
    testCallData <- CallDataPath.fromCallPointPath callPointPath mainPtr
    args <- Traverse.arguments testCallData
    return $ map CallDataPath.toCallPointPath args


getSuccessors :: CallPointPath -> Session [CallPointPath]
getSuccessors callPointPath = do
    mainPtr <- gets $ view Env.mainPtr
    testCallData <- CallDataPath.fromCallPointPath callPointPath mainPtr
    successors <- Traverse.next testCallData
    return $ map CallDataPath.toCallPointPath successors


main :: IO ()
main = hspec spec

shouldBe' :: (Show a, Eq a, MonadIO m) => a -> a -> m ()
shouldBe' = liftIO .: shouldBe


spec :: Spec
spec = do
    describe "interpreter" $ do
        mapM_ (\(name, code) -> it ("executes example - " ++ name) $ do
            --rootLogger setIntLevel 5
            Common.runSession code Executor.processMain) SampleCodes.sampleCodes

    describe "AST traverse" $ do
        it "finds function arguments" $ do
            --rootLogger setIntLevel 5
            Common.runSession SampleCodes.traverseExample $ do
                let var_a    = [CallPoint 1 6 ]
                    var_b    = [CallPoint 1 10]
                    var_c    = [CallPoint 1 21]
                    var_d    = [CallPoint 1 15, CallPoint 1 53]
                    var_e    = [CallPoint 1 15, CallPoint 1 38]

                    testCall = [CallPoint 1 15, CallPoint 1 45, CallPoint 1 71]

                    tuple    = [CallPoint 1 15, CallPoint 1 45, CallPoint 1 (-68)]
                varAArgs  <- getArgs var_a
                varAArgs `shouldBe'` []
                varBArgs  <- getArgs var_b
                varBArgs `shouldBe'` []
                varCArgs  <- getArgs var_c
                varCArgs `shouldBe'` []
                varDArgs  <- getArgs var_d
                varDArgs `shouldBe'` []
                varEArgs  <- getArgs var_e
                varEArgs `shouldBe'` []
                testArgs  <- getArgs testCall
                testArgs  `shouldBe'` [var_c, var_d, var_a, var_b, var_e]
                tupleArgs <- getArgs tuple
                tupleArgs `shouldBe'` [var_e, var_d, var_c, var_b, testCall, var_a]


        it "finds node successors" $ do
            --putStrLn =<< ppShow <$> Common.readCode SampleCodes.traverseExample
            Common.runSession SampleCodes.traverseExample $ do
                let var_a    = [CallPoint 1 6 ]
                    var_b    = [CallPoint 1 10]
                    var_c    = [CallPoint 1 21]
                    var_d    = [CallPoint 1 15, CallPoint 1 53]
                    var_e    = [CallPoint 1 15, CallPoint 1 38]

                    testCall = [CallPoint 1 15, CallPoint 1 45, CallPoint 1 71]

                    tuple    = [CallPoint 1 15, CallPoint 1 45, CallPoint 1 (-68)]
                varASuccs  <- getSuccessors var_a
                varASuccs  `shouldBe'` [testCall, tuple]

            pending
