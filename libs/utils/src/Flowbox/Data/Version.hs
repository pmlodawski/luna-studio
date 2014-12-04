---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Data.Version (
    module Flowbox.Data.Version,
    module X,
) where

import           Data.Aeson
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Version as X
import           GHC.Generics

import Flowbox.Prelude



data Versioned el = Versioned { base    :: el
                              , version :: Version
                              } deriving (Read, Eq, Generic, Ord)


partition :: Int -> [Version] -> Map [Int] [Version]
partition i = foldl (\m xs -> Map.insertWith (++) (take i $ versionBranch xs) [xs] m) mempty


------------------------------------------------------------------------
-- INSTANCES
------------------------------------------------------------------------

deriving instance Generic Version


instance Show el => Show (Versioned el) where
    show (Versioned el v) = show el ++ "-" ++ show v


instance Default Version where
    def = Version { versionBranch = [0,1,0]
                  , versionTags   = def
                  }

instance ToJSON Version
instance FromJSON Version

