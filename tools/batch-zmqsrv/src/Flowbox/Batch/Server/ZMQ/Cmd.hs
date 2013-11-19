---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.ZMQ.Cmd where

import           Flowbox.Prelude   



data Cmd = Serve { address            :: String
                 , port               :: Int
                 
                 , verbose            :: Int
                 , noColor            :: Bool
               
                 , shutdownWithClient :: Bool
                 }
         | Version
         deriving Show
