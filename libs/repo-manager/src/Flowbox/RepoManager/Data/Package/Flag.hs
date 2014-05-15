---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.Data.Package.Flag where

import Flowbox.Prelude
import Data.Function   (on)

data Flag = Flag { name        :: String
                 , description :: String
                 } deriving Show

instance Eq Flag where
    f1 == f2 = name f1 == name f2

instance Ord Flag where
    compare = compare `on` name