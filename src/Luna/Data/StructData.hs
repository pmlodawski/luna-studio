---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances  #-}

module Luna.Data.StructData where

import           Flowbox.Prelude
import qualified Luna.Data.ImportInfo      as II
import           Luna.Data.ImportInfo      (ImportInfo)
import qualified Luna.Data.Namespace       as NM
import           Luna.Data.Namespace       (Namespace)
import qualified Luna.Data.Namespace.State as NMS
import           Luna.Data.StructInfo      (StructInfoMonad)
import qualified Luna.Data.StructInfo      as SI
import           Control.Monad.RWS         (RWST)
import qualified Control.Monad.RWS         as RWST
import           Control.Monad.Trans.Class (lift, MonadTrans)

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



------------------------------------------------------------------------
---- Instances
------------------------------------------------------------------------

instance Monoid StructData where
    mempty      = StructData  mempty mempty
    mappend a b = StructData (mappend (a ^. namespace)  (b ^. namespace))
                             (mappend (a ^. importInfo) (b ^. importInfo))


-- instance (Monad m, Monoid w) => StructDataMonad (RWST r w StructData m) where
--     get = RWST.get
--     put = RWST.put

-- instance (MonadTrans t, StructDataMonad m, Monad m) => StructDataMonad (t m) where
--     get = lift get
--     put = lift . put

-- instance (Monad m, StructDataMonad m) => NM.NamespaceMonad m where
--     get = do 
--         sd <- get
--         return $ sd ^. namespace
--     put i = do
--         sd <- get
--         put (sd & namespace .~ i)
