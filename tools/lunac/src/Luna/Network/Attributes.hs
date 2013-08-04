---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Attributes (
    Attributes,
    empty
) where

import qualified Data.Map as Map
import           Data.Map   (Map)

type Attributes = Map String String -- TODO [PM] implement Attributes

empty :: Attributes
empty = Map.empty
