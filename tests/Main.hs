---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Flowbox.Generics.Deriving.FShow
import           Flowbox.System.Log.Logger       as Logger
import           GHC.Generics
import           Prelude                         hiding (error)

import qualified Flowbox.System.Console.StyledText.StyledText as Text

data Test = Test Int Int deriving(Show, Generic)

instance FShow Test

logger :: Logger
logger   = getLogger "MyApp.BuggyComponent"

loggerIO :: LoggerIO
loggerIO = getLoggerIO "MyApp.BuggyComponent"

test_logger :: IO ()
test_logger = runLogger $ do
    logger debug      "debug"
    logger info       "info"
    logger warning    "warning"
    logger error      "error"
    logger critical   "critical"


f :: String -> String
f x = x

main :: IO ()
main = do
    let t = Test 5 5
    putStrLn $ fshow f t

    logger setLevel DEBUG
    test_logger
    loggerIO error "IO error"

    print "colored text test"
    let x = "ala"
        y = "ola"
    Text.print (x ++ Text.green y ++ x)

    return ()
