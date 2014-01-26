---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Data.Version where

import           Data.Aeson
import           Data.Default      (Default, def)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.String.Utils (join)
import           Flowbox.Prelude
import           GHC.Generics

data Version = Version { branch :: [Int]
                       , tags   :: [String]
                       } deriving (Read, Show, Eq, Generic, Ord)


partition :: Int -> [Version] -> Map [Int] [Version]
partition i x = foldl (\m xs -> Map.insertWith (++) (take i $ branch xs) [xs] m) mempty x

readable :: Version -> String
readable v = readableBranch (branch v) ++ join "" (map ("-"++) $ tags v)

readableBranch :: [Int] -> String
readableBranch b = join "." (map show b)

------------------------------------------------------------------------
-- INSTANCES
------------------------------------------------------------------------

instance Default Version where
    def = Version { branch = [0,1,0]
                  , tags   = def
                  }

instance ToJSON Version
instance FromJSON Version

