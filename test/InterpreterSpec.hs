---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE QuasiQuotes #-}

module InterpreterSpec where

import Control.Monad.State hiding (mapM, mapM_)
import Test.Hspec
import Text.RawString.QQ
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



rootLogger :: Logger
rootLogger = getLogger "Flowbox"



simpleExample :: String
simpleExample = [r|

def main:
    hello = "hello"
    world = "world"
    print hello
    print world
|]

traverseExample :: String
traverseExample = [r|

def main:
    a = "var a"
    b = "var b"
    r = self.foo a b "var c"
    print r
    "dummy"

def foo arg1 arg2 arg3:
    e = "var e"
    n = "var n"
    self.bar arg1 arg2 arg3 "var d" e

def bar arg1 arg2 arg3 arg4 arg5:
    r = test arg3 arg4 arg1 arg2 arg5

    {arg5, arg4, arg3, arg2, r, arg1}

|]

notWorkingCode :: String
notWorkingCode = [r|

def main:
    a = "var a"
    b = "var b"
    r = self.foo a b "var c"
    print r
    "dummy"

def foo arg1 arg2 arg3:
    e = "var e"
    n = "var n"
    self.bar arg1 arg2 arg3 "var d" e

def bar arg1 arg2 arg3 arg4 arg5:
    tuple = self.mkTuple arg1 arg2 arg3 arg4 arg5
    print tuple
    tuple

def mkTuple arg1 arg2 arg3 arg4 arg5:
    {arg1, arg2, arg3, arg4, arg5}
|]


getArgs :: CallPointPath -> Session [CallPointPath]
getArgs callPointPath = do
    mainPtr <- gets $ view Env.mainPtr
    testCallData <- CallDataPath.fromCallPointPath callPointPath mainPtr
    args <- Traverse.arguments testCallData
    return $ map CallDataPath.toCallPointPath args


main :: IO ()
main = hspec spec

shouldBe' :: (Show a, Eq a, MonadIO m) => a -> a -> m ()
shouldBe' = liftIO .: shouldBe


spec :: Spec
spec = do
    describe "interpreter" $ do
        it "executes simple example" $ do
            --rootLogger setIntLevel 5
            Common.runSession simpleExample Executor.processMain

        it "executes more complicated example" $ do
            --rootLogger setIntLevel 5
            Common.runSession notWorkingCode Executor.processMain



    describe "AST traverse" $ do
        it "finds function arguments" $ do
            Common.runSession traverseExample $ do
                let var_a    = [CallPoint 1 6 ]
                    var_b    = [CallPoint 1 10]
                    var_c    = [CallPoint 1 21]
                    var_d    = [CallPoint 1 15, CallPoint 1 53]
                    var_e    = [CallPoint 1 15, CallPoint 1 38]

                    testCall = [CallPoint 1 15, CallPoint 1 45, CallPoint 1 71]

                    tuple    = [CallPoint 1 15, CallPoint 1 45, CallPoint 1 (-68)]
                testArgs  <- getArgs testCall
                testArgs  `shouldBe'` [var_c, var_d, var_a, var_b, var_e]
                tupleArgs <- getArgs tuple
                tupleArgs `shouldBe'` [var_e, var_d, var_c, var_b, testCall, var_a]


        it "finds node successors" $ do
            --putStrLn =<< ppShow <$> Common.readCode traverseExample
            pending
