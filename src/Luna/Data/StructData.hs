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
import qualified Luna.Data.ImportInfo      as II
import           Luna.Data.ImportInfo      (ImportInfo)
import qualified Luna.Data.Namespace       as NS
import           Luna.Data.Namespace       (Namespace)
import qualified Luna.Data.Namespace.State as NMS
import           Luna.Data.StructInfo      (StructInfoMonad)
import qualified Luna.Data.StructInfo      as SI
import           Control.Monad.RWS         (RWST)
import qualified Control.Monad.RWS         as RWST
import           Control.Monad.Trans.Class (lift, MonadTrans)

import qualified Control.Monad.State.Lazy  as State

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------
data StructData = StructData { _namespace  :: Namespace
	                     , _importInfo :: ImportInfo
                             } 

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
    ns <- NS.get
    NS.put $ f ns

modifyImportInfo f = do
    ii <- II.get
    II.put $ f ii


setPath path = modifyImportInfo $ II.setPath path


getPath = do
    ii <- II.get
    return $ II.getPath ii


regVarNameLocal id name = do
    ns <- NS.get
    ii <- II.get
    let path = II._path ii
    NMS.regVarName (SI.OriginInfo path id) name



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



------------------------------------------------------------------------------------
--
--instance (MonadTrans t, StructDataMonad m, Monad m) => StructDataMonad (t m) where
--    get = lift get
--    put = lift . put

foo = do
    x <- get
    return x

foo2 = do
    x <- NS.get
    return x

bar = RWST.runRWST foo (mempty :: StructData)
bar2 = RWST.runRWST foo2 (mempty :: StructData)


