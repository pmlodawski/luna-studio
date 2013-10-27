---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Network.Graph.Value where

import           Flowbox.Prelude   



data Value = Value { value :: String }
             deriving (Show, Read, Ord, Eq)

