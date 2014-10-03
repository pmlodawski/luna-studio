---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Luna.Data.AliasInfo where

import Flowbox.Prelude

import GHC.Generics        (Generic)


import           Data.IntMap  (IntMap)
import           Data.Map     (Map)
import           Luna.AST.AST (AST, ID)
import qualified Luna.AST.AST as AST
import qualified Data.Maps    as Map

type IDMap = IntMap


data Error  = LookupError {key :: String}
            deriving (Show, Eq, Generic, Read)


--data VarRel = VarRel { _nameMap :: Map String AST.ID }
--            deriving (Show)

--makeLenses (''VarRel)

data Scope = Scope { _nameMap :: Map String ID }
           deriving (Show, Eq, Generic, Read)

makeLenses (''Scope)


data AliasInfo = AliasInfo  { _scope   :: IDMap Scope
                            , _alias   :: IDMap ID
                            , _orphans :: IDMap Error
                            , _parent  :: IDMap ID
                            , _ast     :: IDMap AST
                            } deriving (Show, Eq, Generic, Read)

makeLenses (''AliasInfo)



regParent id pid  = parent %~ Map.insert id pid
regAST    id a    = ast    %~ Map.insert id a
regName   pid id name info = info & scope.at pid ?~ Scope (nmap & at name ?~ id) where
    nmap = case Map.lookup pid (_scope info) of
        Nothing        -> mempty
        Just (Scope m) -> m

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Monoid Scope where
    mempty      = Scope mempty
    mappend a b = Scope (mappend (a ^. nameMap)  (b ^. nameMap))


instance Monoid AliasInfo where
    mempty      = AliasInfo mempty mempty mempty mempty mempty
    mappend a b = AliasInfo (mappend (a ^. scope)   (b ^. scope))
                            (mappend (a ^. alias)   (b ^. alias))
                            (mappend (a ^. orphans) (b ^. orphans))
                            (mappend (a ^. parent)  (b ^. parent))
                            (mappend (a ^. ast)     (b ^. ast))


instance Default Scope where
    def = Scope def

instance Default AliasInfo where
    def = AliasInfo def def def def def