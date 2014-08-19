---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Data.Version where

import           Data.Aeson
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.String.Utils (join)
import           Flowbox.Prelude
import           GHC.Generics


data Versioned el = Versioned { base :: el
                              , version :: Version 
                              } deriving (Read, Eq, Generic, Ord)

data Version = Version { branch :: [Int]
                       , tags   :: [String]
                       } deriving (Read, Eq, Generic, Ord)


partition :: Int -> [Version] -> Map [Int] [Version]
partition i = foldl (\m xs -> Map.insertWith (++) (take i $ branch xs) [xs] m) mempty

showBranch :: [Int] -> String
showBranch b = join "." (map show b)

------------------------------------------------------------------------
-- INSTANCES
------------------------------------------------------------------------

instance Show Version where
    show v = showBranch (branch v) ++ join "" (map ("-"++) $ tags v)

instance Show el => Show (Versioned el) where
    show (Versioned el v) = show el ++ "-" ++ show v

instance Default Version where
    def = Version { branch = [0,1,0]
                  , tags   = def
                  }

instance ToJSON Version
instance FromJSON Version

