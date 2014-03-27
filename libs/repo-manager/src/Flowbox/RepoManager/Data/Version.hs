---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.Data.Version where

import Flowbox.Prelude



type Version = [Int]


data Range = Range { max :: Maybe Version
                   , min :: Maybe Version
                   } deriving (Show)


data Constrain = Include Version
               | Exclude Version
               deriving (Show)

