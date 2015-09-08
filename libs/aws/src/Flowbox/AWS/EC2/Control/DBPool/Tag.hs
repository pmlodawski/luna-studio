---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Flowbox.AWS.EC2.Control.DBPool.Tag where


import qualified Flowbox.AWS.Tag as Tag



poolKey :: Tag.Key
poolKey = "control"


poolValue :: Tag.Value
poolValue = "dbpool-1.0"

