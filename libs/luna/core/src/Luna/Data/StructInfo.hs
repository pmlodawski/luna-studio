---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Luna.Data.StructInfo where

import Flowbox.Prelude

import GHC.Generics        (Generic)


import           Data.IntMap  (IntMap)
import           Data.Map     (Map)
import           Luna.Syntax.AST (AST, ID)
import qualified Luna.Syntax.AST as AST
import           Luna.Syntax.Decl (Path)
import qualified Data.Maps    as Map
import           Luna.Syntax.Name.Path  (NamePath, QualPath)
import qualified Luna.Syntax.Name.Path  as NamePath
import qualified Flowbox.Data.MapForest as MapForest
import           Flowbox.Data.MapForest (MapForest)
import           Control.Monad          (join)
import           Luna.Syntax.Name.Pattern (NamePatDesc)
import           Control.Monad.RWS         (RWST)
import qualified Luna.Syntax.Module as Module

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

type IDMap = IntMap

type NameMap v = MapForest Text v

data Error  = LookupError { key  :: Text }
            | ImportError { path :: Path, msg :: String }
            deriving (Show, Eq, Generic, Read)


--data NameDesc = NameDesc ID (Maybe NamePatDesc)


data Scope = Scope { _varnames  :: NameMap OriginInfo
                   , _typenames :: NameMap OriginInfo 
                   } deriving (Show, Eq, Generic, Read)




type ScopeID = ID

data OriginInfo = OriginInfo { _mod    :: QualPath
                             , _target :: ID
                             } deriving (Show, Eq, Generic, Read)

data StructInfo = StructInfo { _scope   :: IDMap Scope
                             , _alias   :: IDMap OriginInfo
                             , _orphans :: IDMap Error
                             , _parent  :: IDMap ID
                             , _argPats :: IDMap NamePatDesc
                             } deriving (Show, Eq, Generic, Read)

makeLenses (''Scope)
makeLenses (''OriginInfo)
makeLenses (''StructInfo)

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class StructInfoMonad m where
    get :: m StructInfo
    put :: StructInfo -> m ()


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

regParent  id pid  = parent %~ Map.insert id pid
regVarName pid id name info = setScope info pid $ Scope (vnmap & at name ?~ id) tnmap where
    (vnmap, tnmap) = scopeLookup pid info

regOrphan id err = orphans %~ Map.insert id err

regArgPat id argPat = argPats %~ Map.insert id argPat

regTypeName pid id name info = setScope info pid $ Scope vnmap (tnmap & at name ?~ id) where
    (vnmap, tnmap) = scopeLookup pid info

setScope info id s = info & scope.at id ?~ s

scopeLookup pid info = case Map.lookup pid (_scope info) of
        Nothing          -> (mempty, mempty)
        Just (Scope v t) -> (v,t)

regAlias :: ID -> NamePath -> ScopeID -> StructInfo -> StructInfo
regAlias id name scopeID structInfo = case mvid of
    Just vid -> structInfo & alias   . at id ?~ vid
    Nothing  -> structInfo & orphans . at id ?~ (LookupError $ toText name)
    where vnames = structInfo ^? scope . ix scopeID . varnames
          mvid   = join $ fmap (MapForest.lookup $ NamePath.toList name) vnames

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Monoid Scope where
    mempty      = Scope mempty mempty
    mappend a b = Scope (mappend (a ^. varnames)  (b ^. varnames))
                        (mappend (a ^. typenames) (b ^. typenames))


instance Monoid StructInfo where
    mempty      = StructInfo mempty mempty mempty mempty mempty
    mappend a b = StructInfo (mappend (a ^. scope)   (b ^. scope))
                             (mappend (a ^. alias)   (b ^. alias))
                             (mappend (a ^. orphans) (b ^. orphans))
                             (mappend (a ^. parent)  (b ^. parent))
                             (mappend (a ^. argPats) (b ^. argPats))


instance Default Scope where
    def = mempty

instance Default StructInfo where
    def = mempty



--instance StructInfoMonad (RWST ) where
--    func = 
