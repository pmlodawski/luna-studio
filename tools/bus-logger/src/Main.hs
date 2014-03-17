---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import qualified Flowbox.Bus.Env           as Env
import qualified Flowbox.Bus.Logger        as Logger
import           Flowbox.Bus.Topic         (Topic)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.Logger"


--TODO [PM] : remove
ep :: Env.BusEndPoints
ep = Env.BusEndPoints "tcp://127.0.0.1:30530"
                      "tcp://127.0.0.1:30531"
                      "tcp://127.0.0.1:30532"

topics :: [Topic]
topics = [""]
--


main :: IO ()
main = do rootLogger setLevel TRACE
          r <- Logger.run ep topics
          case r of
              Left err -> logger criticalFail err
              _        -> return ()


