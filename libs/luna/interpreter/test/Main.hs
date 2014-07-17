---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Main where

import Data.EitherR (fmapL)

import           Flowbox.Control.Error                                 (eitherStringToM)
import qualified Flowbox.Interpreter.Session.AST.Executor              as Executor
import qualified Flowbox.Interpreter.Session.Cache.Invalidate          as Invalidate
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
             [ ""
             , "def test self arg arg2:"
             , "    print arg"
             , "    print arg2"
             , "    self.bla \"kota\" \"albo nie\""
             , ""
             , "def bla self arg arg2:"
             , "    a = \"grubego\""
             , "    b = print a"
             , "    {arg, arg2, b}"
             , ""
             , "def main self:"
             , "    a = self.test \"ala\" \"ma\""
             , "    print a"
             , "    \"dummy\""
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
        --Invalidate.invalidateDef  libID 2
        Invalidate.invalidateNode libID 10
        --Invalidate.invalidate [CallPoint libID 52, CallPoint libID 17, CallPoint libID 38]
        Executor.processMain
        Executor.processMain
    eitherStringToM $ fmapL Error.format result
