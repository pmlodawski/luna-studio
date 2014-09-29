---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Main where

import Flowbox.Generics.Deriving.FShow
import Flowbox.System.Log.Logger       as Logger
import GHC.Generics
import Prelude                         hiding (error)

import           Flowbox.Source.Location
import qualified Flowbox.System.Console.StyledText.StyledText as Text


data Test = Test Int Int deriving(Show, Generic)

instance FShow Test

logger :: Logger
logger   = getLogger "MyApp.BuggyComponent"

loggerIO :: LoggerIO
loggerIO = getLoggerIO "MyApp.BuggyComponent"

testLogger :: IO ()
testLogger = runLogger $ do
    logger debug      "debug"
    logger info       "info"
    logger warning    "warning"
    logger error      "error"
    logger critical   "critical"


f :: String -> String
f x = x

main :: IO ()
main = do
    putStrLn $ format $loc
    let t = Test 5 5
    putStrLn $ fshow f t

    logger setLevel DEBUG
    testLogger
    loggerIO error "IO error"

    print ("colored text test" :: String)
    let x = "ala"
        y = Text.green "ola"
        txt = x ++ y ++ x
    Text.print txt
    Text.print $ Text.clearFormatting txt
    print $ Text.toText txt

    return ()
