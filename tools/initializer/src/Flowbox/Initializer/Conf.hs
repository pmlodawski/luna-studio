---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Initializer.Conf where

import           Flowbox.Prelude   



data Conf = Initialization { verbose :: Bool
                           , force   :: Bool
                           }
          | Version
          deriving Show
