---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Data.StructData where

import           Control.Monad.RWS         (RWST)
import qualified Control.Monad.RWS         as RWST
import qualified Control.Monad.State.Lazy  as State
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map

import           Flowbox.Prelude
import           Luna.Data.ImportInfo      (ImportInfo)
import qualified Luna.Data.ImportInfo      as II
import           Luna.Data.ModuleInfo      (ImportError (..))
import qualified Luna.Data.ModuleInfo      as MI
import           Luna.Data.Namespace       (Namespace)
import qualified Luna.Data.Namespace       as NS
import qualified Luna.Data.Namespace.State as NMS
import           Luna.Data.StructInfo      (StructInfoMonad)
import qualified Luna.Data.StructInfo      as SI
import           Luna.Syntax.Name.Path     (QualPath)



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

--CR[PM->TD] : add type signature
--CR[PM->TD] : "do" is not required
modify f = do
--CR[PM->TD] : use modify
    sd <- get
    put $ f sd

--CR[PM->TD] : add type signature
--CR[PM->TD] : "do" is not required
modifyNamespace f = do
--CR[PM->TD] : use modify
    StructData ns ii <- get
    put $ StructData (f ns) ii

--CR[PM->TD] : add type signature
--CR[PM->TD] : "do" is not required
modifyImportInfo f = do
--CR[PM->TD] : use modify
    StructData ns ii <- get
    put $ StructData ns (f ii)


--CR[PM->TD] : add type signature
setPath path = modifyImportInfo $ II.setPath path


--CR[PM->TD] : add type signature
--CR[PM->TD] : "do" is not required
getPath = do
--CR[PM->TD] : use <$>
    ii <- II.get
    return $ II.getPath ii



--CR[PM->TD] : add type signature
regNameLocal tag id name = do
    ii <- II.get
    let path       = ii ^. II.path
        originInfo = (SI.OriginInfo path id)
        regFun     = case tag of
            II.Vars  -> NMS.regVarName
            II.Types -> NMS.regTypeName
    regFun originInfo name
    case tag of
        II.Vars ->  modifyImportInfo $ II.symTable  %~ (Map.insert name [originInfo]) -- TODO[PMo] make inserting function
        II.Types -> modifyImportInfo $ II.typeTable %~ (Map.insert name [originInfo])


--CR[PM->TD] : add type signature
regVarNameLocal = regNameLocal II.Vars

--CR[PM->TD] : add type signature
regTypeNameLocal = regNameLocal II.Types



--CR[PM->TD] : add type signature
regName tag id name = do
    ii <- II.get
    let path    = ii ^. II.path
        nameMap = (case tag of II.Vars -> ii ^. II.symTable; II.Types -> ii ^. II.typeTable)
    case (Map.lookup name nameMap) of
        Just [origin]    -> regOrigin id origin
        Just mods@(o:os) -> regError (AmbRefError name (getOriginPaths mods))
        _                -> NMS.regAlias id name --regOrphan id (SI.LookupError $ toText name)


--CR[PM->TD] : add type signature
regVarName = regName II.Vars

--CR[PM->TD] : add type signature
regTypeName = regName II.Types



getOriginPaths :: [SI.OriginInfo] -> [QualPath]
getOriginPaths infos = map f infos
    where f (SI.OriginInfo m _) = m


--CR[PM->TD] : add type signature
--CR[PM->TD] : "do" is not required
regOrigin id origin = do
    modifyNamespace $ NS.regOrigin id origin


--CR[PM->TD] : add type signature
--CR[PM->TD] : "do" is not required
regOrphan id err = do
    modifyNamespace $ NS.regOrphan id err


--CR[PM->TD] : add type signature
--CR[PM->TD] : "do" is not required
regError err = do
    modifyImportInfo $ II.regError err

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
--CR[PM->TD] : use <$> and "view"
    get = do
        sd <- get
        return $ sd ^. namespace
--CR[PM->TD] : use "modify" and %~
    put i = do
        sd <- get
        put (sd & namespace .~ i)


instance (Monad m, Monoid w) => II.ImportInfoMonad (RWST r w StructData m) where
--CR[PM->TD] : use <$> and "view"
    get = do
        sd <- get
        return $ sd ^. importInfo
--CR[PM->TD] : use "modify" and %~
    put i = do
        sd <- get
        put (sd & importInfo .~ i)
