---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Data.Version where

import           Flowbox.Prelude   
import           GHC.Generics

data Version = Version { branch :: [Int]
                       , stage  :: Stage
                       } deriving (Show, Eq, Generic)

data Stage = PreAlpha
           | Alpha
           | Beta
           | RC Int
           | Final
           deriving (Show, Eq, Generic)