---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Data.StructData where

import           Flowbox.Prelude
import qualified Data.Map                  as Map
import           Data.Map                  (Map)
import qualified Luna.Data.ImportInfo      as II
import           Luna.Data.ImportInfo      (ImportInfo)
import qualified Luna.Data.ModuleInfo      as MI
import           Luna.Data.ModuleInfo      (ImportError(..))
import qualified Luna.Data.Namespace       as NS
import           Luna.Data.Namespace       (Namespace)
import qualified Luna.Data.Namespace.State as NMS
import           Luna.Data.StructInfo      (StructInfoMonad)
import qualified Luna.Data.StructInfo      as SI
import           Luna.Syntax.Name.Path     (QualPath)
import           Control.Monad.RWS         (RWST)
import qualified Control.Monad.RWS         as RWST
import           Control.Monad.Trans.Class (lift, MonadTrans)

import qualified Control.Monad.State.Lazy  as State

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------
data StructData = StructData { _namespace  :: Namespace
	                     , _importInfo :: ImportInfo
                             } deriving Show

makeLenses ''StructData

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------
class StructDataMonad m where
    get :: m StructData
    put :: StructData -> m ()

----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

modify f = do
    sd <- get
    put $ f sd

modifyNamespace f = do
    StructData ns ii <- get
    put $ StructData (f ns) ii

modifyImportInfo f = do
    StructData ns ii <- get
    put $ StructData ns (f ii)


setPath path = modifyImportInfo $ II.setPath path


getPath = do
    ii <- II.get
    return $ II.getPath ii



regNameLocal tag id name = do
    ii <- II.get
    let path       = II._path ii
        originInfo = (SI.OriginInfo path id)
        regFun     = case tag of
            II.Vars  -> NMS.regVarName
            II.Types -> NMS.regTypeName
    regFun originInfo name
    case tag of
        II.Vars ->  modifyImportInfo $ II.symTable  %~ (Map.insert name [originInfo]) -- TODO[PMo] make inserting function
        II.Types -> modifyImportInfo $ II.typeTable %~ (Map.insert name [originInfo])


regVarNameLocal = regNameLocal II.Vars

regTypeNameLocal = regNameLocal II.Types



regName tag id name = do
    ii <- II.get
    let path    = II._path ii
        nameMap = (case tag of II.Vars -> II._symTable; II.Types -> II._typeTable) ii
    case (Map.lookup name nameMap) of
        Just [origin]    -> regOrigin id origin
        Just mods@(o:os) -> regError (AmbRefError name (getOriginPaths mods))
        _                -> NMS.regAlias id name --regOrphan id (SI.LookupError $ toText name)


regVarName = regName II.Vars

regTypeName = regName II.Types



getOriginPaths :: [SI.OriginInfo] -> [QualPath]
getOriginPaths infos = map f infos
    where f (SI.OriginInfo m _) = m


regOrigin id origin = do
    modifyNamespace $ NS.regOrigin id origin


regOrphan id err = do
    modifyNamespace $ NS.regOrphan id err


regError err = do
    modifyImportInfo $ II.regError err

-- wrapper for the regVarName from Namespace, sets the appropriate path
--regVarName id name = do
--    ns <- NS.get
--    s  <- NS.getCurrentScope
--    case s of
--        Just scope -> case SI.lookupVarInScope of
--            Just originInfo -> NS.regVarName originInfo

----------------------------------------------------------------------
---- Instances
----------------------------------------------------------------------

instance Monoid StructData where
    mempty      = StructData  mempty mempty
    mappend a b = StructData (mappend (a ^. namespace)  (b ^. namespace))
                             (mappend (a ^. importInfo) (b ^. importInfo))


instance (Monad m, Monoid w) => StructDataMonad (RWST r w StructData m) where
    get = RWST.get
    put = RWST.put


instance (Monad m, Monoid w) => NS.NamespaceMonad (RWST r w StructData m) where
    get = do
        sd <- get
        return $ sd ^. namespace
    put i = do
        sd <- get
        put (sd & namespace .~ i)


instance (Monad m, Monoid w) => II.ImportInfoMonad (RWST r w StructData m) where
    get = do
        sd <- get
        return $ sd ^. importInfo
    put i = do
        sd <- get
        put (sd & importInfo .~ i)



----------------------------------------------------------------------
---- Some funs to check if Monads implemented properly ;)
----------------------------------------------------------------------

foo = do
    x <- get
    return x

foo2 = do
    x <- NS.get
    return x

bar = RWST.runRWST foo (mempty :: StructData)
bar2 = RWST.runRWST foo2 (mempty :: StructData)
