---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Luna.Data.Analysis.Alias.Alias where

import Flowbox.Prelude

import           Data.IntMap                 (IntMap)
import           Data.Map                    (Map)
import qualified Flowbox.Luna.Data.AST.Utils as AST


type ID = AST.ID

data Error  = LookupError {key :: String}
            deriving (Show)


data VarRel = VarRel { _nameMap :: Map String AST.ID }
            deriving (Show)

makeLenses (''VarRel)


data AA     = AA  { _varRel    :: IntMap VarRel
                  , _aliasMap  :: IntMap (Either Error AST.ID)
                  , _parentMap :: IntMap AST.ID
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

