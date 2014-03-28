---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.Data.Version (
    module Data.Version,
    Range(..),
    Constraint(..),
) where

import Data.Version

import Flowbox.Prelude




data Range = Range { min :: Maybe Version
                   , max :: Maybe Version
                   } deriving (Show)


data Constraint = Include Range
                | Exclude Range
                deriving (Show)




