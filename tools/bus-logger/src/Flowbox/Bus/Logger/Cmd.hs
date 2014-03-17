---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Bus.Logger.Cmd where

import qualified Flowbox.Bus.Env as Env
import           Flowbox.Prelude



data Cmd = Run { topics       :: [String]

               , ctrlEndPoint :: String
               , pullEndPoint :: String
               , pubEndPoint  :: String

               , verbose      :: Int
               , noColor      :: Bool
               }
         | Version
         deriving Show


endPoints :: Cmd -> Env.BusEndPoints
endPoints cmd = Env.BusEndPoints (ctrlEndPoint cmd)
                                 (pullEndPoint cmd)
                                 (pubEndPoint  cmd)
