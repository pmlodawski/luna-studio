---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Animation.Model where

import Data.Map

import Flowbox.Prelude

data NonNumericModel x y = NonNumericModel { _segments :: Map x (Maybe y) }

makeLenses ''NonNumericModel

