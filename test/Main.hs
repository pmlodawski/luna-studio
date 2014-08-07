---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad.IO.Class (MonadIO)
import Data.EitherR           (fmapL)
import Text.RawString.QQ
import Text.Show.Pretty

import           Flowbox.Control.Error                                 (eitherStringToM)
import qualified Flowbox.Interpreter.Session.AST.Executor              as Executor
import qualified Flowbox.Interpreter.Session.Cache.Cache               as Cache
import qualified Flowbox.Interpreter.Session.Cache.Invalidate          as Invalidate
import           Flowbox.Interpreter.Session.Data.DefPoint             (DefPoint (DefPoint))
import qualified Flowbox.Interpreter.Session.Env                       as Env
import qualified Flowbox.Interpreter.Session.Error                     as Error
import qualified Flowbox.Interpreter.Session.Session                   as Session
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb                     as Crumb
import           Flowbox.Luna.Data.Pass.Source                         (Source (Source))
import qualified Flowbox.Luna.Data.PropertyMap                         as PropertyMap
import           Flowbox.Luna.Lib.LibManager                           (LibManager)
import qualified Flowbox.Luna.Lib.LibManager                           as LibManager
import           Flowbox.Luna.Lib.Library                              (Library (Library))
import qualified Flowbox.Luna.Lib.Library                              as Library
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser as TxtParser
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.UniPath                                as UniPath



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Test"


code :: Source
code = Source ["Main"] $ [r|
def test self arg arg2:
    print arg
    print arg2
    self.bla "kota" "albo nie"

def bla self arg arg2:
    a = "grubego"

    {arg, arg2, print a}

def main self:
    a = self.test "ala" "ma"
    print a
    "dummy"
|]

code2 :: Source
code2 = Source ["Main"] $ [r|
def test self arg arg2:
    print arg
    print arg2
    self.bla "kota" "albo nie"

def bla self arg arg2:
    a = "grubego"

    {arg, arg2, print a}

def main self:
    a = self.test "ala2" "ma"
    print a
    "dummy"
|]

readSource :: (Control.Monad.IO.Class.MonadIO m, Functor m)
           => Source -> m (LibManager, Library.ID)
readSource source = do
    let path = UniPath.fromUnixString "."
    (ast, _, _) <- eitherStringToM =<< TxtParser.run source
    return $ LibManager.insNewNode (Library "Main" path ast PropertyMap.empty)
           $ LibManager.empty


main :: IO ()
main = do
    rootLogger setIntLevel 5

    (libManager , libID) <- readSource code
    (libManager2, _    ) <- readSource code2

    let env = Env.mk libManager 0 (DefPoint libID [Crumb.Module "Main", Crumb.Function "main" []])

    putStrLn $ ppShow $ LibManager.lab libManager libID

    result <- Session.run env $ do
        Executor.processMain
        putStrLn "--------- 1"
        Executor.processMain
        putStrLn "========= 1"

        --Invalidate.invalidateDef  libID 2
        Cache.dumpAll
        Invalidate.modifyNode libID 51
        Cache.dumpAll

        --Invalidate.invalidate [CallPoint libID 52, CallPoint libID 17, CallPoint libID 38]
        Executor.processMain
        putStrLn "--------- 2"
        Executor.processMain

        putStrLn "========= 2"
        Cache.dumpAll
        Session.setLibManager libManager2
        Invalidate.modifyNode libID 51
        Cache.dumpAll
        Executor.processMain
        Cache.dumpAll
    eitherStringToM $ fmapL Error.format result
