---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Main where

import Data.EitherR (fmapL)

import           Flowbox.Control.Error                                 (eitherStringToM)
import qualified Flowbox.Interpreter.Error                             as Error
import qualified Flowbox.Interpreter.Mockup.Graph                      as Graph
import           Flowbox.Interpreter.Mockup.Node                       (Node (Node))
import           Flowbox.Interpreter.Mockup.Type                       (Type (Type))
import qualified Flowbox.Interpreter.Session.Cache                     as Cache
import qualified Flowbox.Interpreter.Session.Env                       as Env
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



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Test"


main :: IO ()
main = do
    rootLogger setIntLevel 5
    let graph = Graph.mkGraph
                    [ (0, Node "15"    $ Type "Int")
                    , (1, Node "12"    $ Type "Int")
                    , (2, Node "94"    $ Type "Int")
                    , (3, Node "(+)"   $ Type "Int -> Int -> Int")
                    , (4, Node "(*)"   $ Type "Int -> Int -> Int")
                    , (5, Node "print" $ Type "Int -> IO ()")
                    ]
                    [ (0, 3, Graph.Dependency)
                    , (1, 3, Graph.Dependency)
                    , (2, 4, Graph.Dependency)
                    , (3, 4, Graph.Dependency)
                    , (4, 5, Graph.Dependency)
                    ]

        code = Source ["Main"] $ unlines
             [ "def foo:"
             , "    print \"foo\""
             , ""
             , "def bar:"
             , "    print \"bar\""
             , ""
             , "def main:"
             , "    print \"hello\""
             , "    foo"
             , "    print \"world\""
             ]
        path = UniPath.fromUnixString "."

    (source, _, _) <- eitherStringToM =<< TxtParser.run code

    let (libManager, libID)
            = LibManager.insNewNode (Library "main" path source PropertyMap.empty)
            $ LibManager.empty

        env = Env.mk libManager (libID, [Crumb.ModuleCrumb "Main", Crumb.FunctionCrumb "main" []])

    print env

    result <- Session.run env $ do
        Cache.findMain
        --mapM_ (const $ Cache.runNode graph 4) [0..10000]
        --mapM_ (const $ Cache.runNodeIfNeeded graph 4) [0..1000000]
        --Cache.runNodeIfNeeded graph 5
        --Cache.dump 3
        --Cache.dump 4
        Cache.invalidate      graph [(libID, 0)]
        --Cache.runNodeIfNeeded graph 5
        --Cache.runNodeIfNeeded graph 5
    eitherStringToM $ fmapL Error.format result
