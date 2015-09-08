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

import           Control.Arrow                (first)
import           Data.Aeson
import           Data.Binary                  (Binary)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Version                 as X
import           GHC.Generics
import qualified Text.ParserCombinators.ReadP as ReadP

import Flowbox.Prelude



newtype Name = Name String


data Versioned el = Versioned { base    :: el
                              , version :: Version
                              } deriving (Eq, Generic, Ord)


partition :: Int -> [Version] -> Map [Int] [Version]
partition i = foldl (\m xs -> Map.insertWith (++) (take i $ versionBranch xs) [xs] m) mempty


readVersionMaybe :: String -> Maybe Version
readVersionMaybe s = case [ x | (x,"") <- ReadP.readP_to_S parseVersion $ s] of
    [x] -> Just x
    _   -> Nothing

------------------------------------------------------------------------
-- INSTANCES
------------------------------------------------------------------------

instance ToString Name where
  toString (Name name) = name

instance Show Name where
    show = toString

instance Read Name where
    readsPrec n = map (first Name) . readsPrec n . quote
        where quote s = '"' : s ++ ['"']

instance Show el => Show (Versioned el) where
    show (Versioned el v) = show el ++ "-" ++ showVersion v

instance Default Version where
    def = Version { versionBranch = [0,1,0]
                  , versionTags   = def
                  }

instance ToJSON Version
instance FromJSON Version
