---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Luna.Data.Analysis.Alias.Alias where

import Flowbox.Prelude

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Map    (Map)
import qualified Data.Map    as Map


type ID = Int

data Error  = LookupError {key :: String}
            deriving (Show)


data AA     = AA  { _nameMap  :: Map String ID
                  , _aliasMap :: IntMap (Either Error ID) 
                  } 
            deriving (Show)
makeLenses (''AA)


instance Monoid AA where
	mempty = AA mempty mempty

