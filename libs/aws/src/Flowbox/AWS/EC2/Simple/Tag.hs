---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Flowbox.AWS.EC2.Simple.Tag where

import qualified Flowbox.AWS.Tag as Tag



simpleKey :: Tag.Key
simpleKey = "control"


simpleValue :: Tag.Value
simpleValue = "simple-1.0"

