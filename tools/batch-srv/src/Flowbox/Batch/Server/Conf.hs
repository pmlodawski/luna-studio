---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.Conf where

import           Flowbox.Prelude   



data Conf = Serve { address            :: String
                  , port               :: String

                  , verbose            :: Bool
                  , debug              :: Bool
                  , noColor            :: Bool
                
                  , shutdownWithClient :: Bool
                  }
          | Version
          deriving Show
