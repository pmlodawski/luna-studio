---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Main where

import Data.EitherR (fmapL)

import           Flowbox.Control.Error               (eitherStringToM)
import qualified Flowbox.Interpreter.Error           as Error
import qualified Flowbox.Interpreter.Mockup.Graph    as Graph
import qualified Flowbox.Interpreter.Session.Cache   as Cache
import qualified Flowbox.Interpreter.Session.Session as Session
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger


rootLogger :: Logger
rootLogger = getLogger "Flowbox"


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Test"


main :: IO ()
main = do
    rootLogger setIntLevel 5
    let graph = Graph.mkGraph
                    [ (0, "(15 :: Int)")
                    , (1, "(12 :: Int)")
                    , (2, "((+) :: Int -> Int -> Int)")
                    , (3, "(print :: Int -> IO ())")
                    ]
                    [ (0, 2, Graph.Dependency)
                    , (1, 2, Graph.Dependency)
                    , (2, 3, Graph.Dependency)
                    ]
    result <- Session.run $ do
        Cache.runNode 0 graph
        Cache.runNode 1 graph
        Cache.runNode 2 graph
        Cache.dump 2
        Cache.invalidate 2
        Cache.dump 2
        Cache.runNode 3 graph
    eitherStringToM $ fmapL Error.format result
