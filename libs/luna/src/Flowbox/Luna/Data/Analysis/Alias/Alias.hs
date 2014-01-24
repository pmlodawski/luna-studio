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


data VarRel = VarRel { _nameMap :: Map String ID }
            deriving (Show)

makeLenses (''VarRel)


data AA     = AA  { _varRel    :: IntMap VarRel
                  , _aliasMap  :: IntMap (Either Error ID) 
                  , _parentMap :: IntMap ID
                  } 
            deriving (Show)

makeLenses (''AA)


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Monoid VarRel where
    mempty      = VarRel mempty
    mappend a b = VarRel (mappend (a ^. nameMap)  (b ^. nameMap)) 


instance Monoid AA where
    mempty      = AA mempty mempty mempty
    mappend a b = AA (mappend (a ^. varRel)    (b ^. varRel)) 
                     (mappend (a ^. aliasMap)  (b ^. aliasMap))
                     (mappend (a ^. parentMap) (b ^. parentMap))

