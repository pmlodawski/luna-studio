---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Network.Attributes (
    Attributes,
    module Data.Map
) where

import           Flowbox.Prelude   
import           Data.Map          



type Attributes = Map String (Map String String)


