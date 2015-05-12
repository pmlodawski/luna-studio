---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Luna.Data.StructInfo where

import GHC.Generics (Generic)


import           Control.Monad     (join)
import           Control.Monad.RWS (RWST)
import           Data.IntMap       (IntMap)
import           Data.Map          (Map)
import qualified Data.Maps         as Map

import           Flowbox.Data.MapForest   (MapForest)
import qualified Flowbox.Data.MapForest   as MapForest
import           Flowbox.Prelude
import           Luna.Syntax.AST          (AST, ID)
import qualified Luna.Syntax.AST          as AST
import           Luna.Syntax.Decl         (Path)
import qualified Luna.Syntax.Enum         as Enum
import qualified Luna.Syntax.Module       as Module
import           Luna.Syntax.Name.Path    (NamePath, QualPath)
import qualified Luna.Syntax.Name.Path    as NamePath
import           Luna.Syntax.Name.Pattern (NamePatDesc)
----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

type IDMap = IntMap

type NameMap v = MapForest Text v

data Error  = LookupError { key :: Text }
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

makeLenses ''Scope
makeLenses ''OriginInfo
makeLenses ''StructInfo

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class StructInfoMonad m where
    get :: m StructInfo
    put :: StructInfo -> m ()


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------
--TODO[PMo] check if it should be id or parent id
regOrigin :: ID -> OriginInfo -> StructInfo -> StructInfo
regOrigin id origin = alias %~ Map.insert id origin


--CR[PM->TD] : add type signature
regParent  id pid  = parent %~ Map.insert id pid

--CR[PM->TD] : add type signature
regVarName pid id name info = setScope info pid $ Scope (vnmap & at name ?~ id) tnmap where
    (vnmap, tnmap) = scopeLookup pid info


--CR[PM->TD] : add type signature
regOrphan id err = orphans %~ Map.insert id err


--CR[PM->TD] : add type signature
regArgPat id argPat = argPats %~ Map.insert id argPat


--CR[PM->TD] : add type signature
regTypeName pid id name info = setScope info pid $ Scope vnmap (tnmap & at name ?~ id) where
    (vnmap, tnmap) = scopeLookup pid info


--CR[PM->TD] : add type signature
setScope info id s = info & scope.at id ?~ s


scopeLookup pid info = case Map.lookup pid (info ^. scope) of
    Nothing          -> (mempty, mempty)
    Just (Scope v t) -> (v,t)


lookupVarInScope :: NamePath -> Scope -> Maybe OriginInfo
lookupVarInScope name scope = MapForest.lookup nameList vnames
    where nameList = NamePath.toList name
          vnames   = scope ^. varnames


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
    mappend a b = StructInfo (mappend (a ^. scope)    (b ^. scope))
                             (mappend (a ^. alias)    (b ^. alias))
                             (mappend (a ^. orphans)  (b ^. orphans))
                             (mappend (a ^. parent)   (b ^. parent))
                             (mappend (a ^. argPats)  (b ^. argPats))


instance Default Scope where
    def = mempty

instance Default StructInfo where
    def = mempty



