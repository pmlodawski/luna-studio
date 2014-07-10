---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Main where

import Data.EitherR (fmapL)

import           Flowbox.Control.Error                                 (eitherStringToM)
import qualified Flowbox.Interpreter.Session.AST.Cache                 as Cache
import qualified Flowbox.Interpreter.Session.AST.Executor              as Executor
import           Flowbox.Interpreter.Session.Data.CallPoint            (CallPoint (CallPoint))
import           Flowbox.Interpreter.Session.Data.DefPoint             (DefPoint (DefPoint))
import qualified Flowbox.Interpreter.Session.Env                       as Env
import qualified Flowbox.Interpreter.Session.Error                     as Error
import qualified Flowbox.Interpreter.Session.Session                   as Session
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb                     as Crumb
import           Flowbox.Luna.Data.Pass.Source                         (Source (Source))
import qualified Flowbox.Luna.Data.PropertyMap                         as PropertyMap
import qualified Flowbox.Luna.Lib.LibManager                           as LibManager
import           Flowbox.Luna.Lib.Library                              (Library (Library))
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser as TxtParser
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.UniPath                                as UniPath
import           Text.Show.Pretty


rootLogger :: Logger
rootLogger = getLogger "Flowbox"


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Test"


main :: IO ()
main = do
    rootLogger setIntLevel 5
    let code = Source ["Main"] $ unlines
             --[ "def foo self:"
             --, "    print \"foo\""
             --, "    2"
             --, ""
             --, "def bar self:"
             --, "    self.foo"
             --, "    print \"bar\""
             --, "    3"
             --, ""
             --, "def main self:"
             --, "    print \"hello\""
             --, "    self.foo"
             --, "    self.bar"
             --, "    print \"world\""
             --, "    1"
             --]
             [ ""
             , "def test self arg arg2:"
             , "    a = self.bla arg arg2"
             , "    a"
             , ""
             , "def bla self arg arg2:"
             , "    b = zupah arg arg2"
             , "    c = fooooo"
             , "    {b,1,b, arg}"
             , ""
             , "def main self:"
             , "    a = self.test 700 900"
             --, "    b = self.test a a"
             , "    print a"
             , "    1"
             ]
        path = UniPath.fromUnixString "."

    (source, _, _) <- eitherStringToM =<< TxtParser.run code

    let (libManager, libID)
            = LibManager.insNewNode (Library "Main" path source PropertyMap.empty)
            $ LibManager.empty

        env = Env.mk libManager (DefPoint libID [Crumb.ModuleCrumb "Main", Crumb.FunctionCrumb "main" []])

    putStrLn $ ppShow $ LibManager.lab libManager libID

    result <- Session.run env $ do
        Executor.processMain
        putStrLn "---------"
        Executor.processMain
        Cache.invalidate [CallPoint libID 50, CallPoint libID 13, CallPoint libID 36]
        --Cache.invalidate [CallPoint libID 60]
        --Cache.invalidate [CallPoint libID 52]
        --putStrLn "---------"
        --Cache.invalidate [CallPoint libID 57]
        --Cache.invalidate [CallPoint libID 47]
        Executor.processMain
        Executor.processMain
        Executor.processMain
        --mapM_ (const $ Cache.runNode graph 4) [0..10000]
        --mapM_ (const $ Cache.runNodeIfNeeded graph 4) [0..1000000]
        --Cache.runNodeIfNeeded graph 5
        --Cache.dump 3
        --Cache.dump 4
        --Cache.invalidate [  (libID, [ Crumb.ModuleCrumb "Main"
        --                            , Crumb.FunctionCrumb "main" []
        --                            ]
        --                  ),(libID, [ Crumb.ModuleCrumb "Main"
        --                            , Crumb.FunctionCrumb "foo"  []
        --                            ]
        --                  )]
        --Cache.runNodeIfNeeded graph 5
        --Cache.runNodeIfNeeded graph 5
    eitherStringToM $ fmapL Error.format result
