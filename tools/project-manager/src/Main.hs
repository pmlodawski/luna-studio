---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import qualified Flowbox.Bus.Client               as Client
import qualified Flowbox.Bus.Env                  as Env
import           Flowbox.Prelude
import qualified Flowbox.ProjectManager.Processor as Processor
import           Flowbox.System.Log.Logger



rootLogger :: Logger
rootLogger = getLogger "Flowbox"

--TODO [PM] : remove
ep :: Env.BusEndPoints
ep = Env.BusEndPoints "tcp://127.0.0.1:30530"
                      "tcp://127.0.0.1:30531"
                      "tcp://127.0.0.1:30532"
--

main :: IO ()
main = do rootLogger setLevel TRACE
          r <- Client.run ep Processor.topic Processor.process
          print r

