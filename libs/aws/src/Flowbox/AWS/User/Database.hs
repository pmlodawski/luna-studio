---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.User.Database (
    module Data.Map,
    Database,
) where

import Data.Map

import qualified Flowbox.AWS.User.User as User



type Database = Map User.Name User.Data
