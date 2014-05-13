---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Flowbox.AWS.EC2.Pool.Tag where

import qualified Flowbox.AWS.Tag as Tag



poolTagKey :: Tag.Key
poolTagKey = "pool"


poolTagValue :: Tag.Value
poolTagValue = "1.0"

