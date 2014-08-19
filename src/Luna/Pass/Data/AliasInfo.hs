---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Luna.Pass.Data.AliasInfo where

import Flowbox.Prelude

import           Data.IntMap               (IntMap)
import           Data.Map                  (Map)
import           Flowbox.Luna.Data.AST.AST (AST)
import qualified Flowbox.Luna.Data.AST.AST as AST


data Error  = LookupError {key :: String}
            deriving (Show)


data VarRel = VarRel { _nameMap :: Map String AST.ID }
            deriving (Show)

makeLenses (''VarRel)


data AliasInfo = AliasInfo  { _varRel     :: IntMap VarRel
                            , _aliasMap   :: IntMap AST.ID
                            , _invalidMap :: IntMap Error
                            , _parentMap  :: IntMap AST.ID
                            , _astMap     :: IntMap AST
                            }
               deriving (Show)

makeLenses (''AliasInfo)


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Monoid VarRel where
    mempty      = VarRel mempty
    mappend a b = VarRel (mappend (a ^. nameMap)  (b ^. nameMap))


instance Monoid AliasInfo where
    mempty      = AliasInfo mempty mempty mempty mempty mempty
    mappend a b = AliasInfo (mappend (a ^. varRel)     (b ^. varRel))
                            (mappend (a ^. aliasMap)   (b ^. aliasMap))
                            (mappend (a ^. invalidMap) (b ^. invalidMap))
                            (mappend (a ^. parentMap)  (b ^. parentMap))
                            (mappend (a ^. astMap)     (b ^. astMap))

