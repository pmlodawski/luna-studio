---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

module Flowbox.Data.Serialization where

import Control.Monad
import Data.Char                        (ord)
import Text.ProtocolBuffers.Basic       (defaultValue)
import Text.ProtocolBuffers.Extensions
import Text.ProtocolBuffers.Identifiers

import           Flowbox.Data.Error                  (Error (Error))
import           Flowbox.Data.Mode                   (Mode)
import           Flowbox.Prelude
import           Generated.Proto.Data.BoolData       (BoolData (BoolData))
import qualified Generated.Proto.Data.BoolData       as BoolData
import           Generated.Proto.Data.CharData       (CharData (CharData))
import qualified Generated.Proto.Data.CharData       as CharData
import           Generated.Proto.Data.DoubleData     (DoubleData (DoubleData))
import qualified Generated.Proto.Data.DoubleData     as DoubleData
import           Generated.Proto.Data.EmptyTupleData (EmptyTupleData (EmptyTupleData))
import qualified Generated.Proto.Data.EmptyTupleData as EmptyTupleData
import           Generated.Proto.Data.ErrorData      (ErrorData (ErrorData))
import qualified Generated.Proto.Data.ErrorData      as ErrorData
import           Generated.Proto.Data.FloatData      (FloatData (FloatData))
import qualified Generated.Proto.Data.FloatData      as FloatData
import           Generated.Proto.Data.IntData        (IntData (IntData))
import qualified Generated.Proto.Data.IntData        as IntData
import           Generated.Proto.Data.StringData     (StringData (StringData))
import qualified Generated.Proto.Data.StringData     as StringData
import           Generated.Proto.Data.Value          (Value (Value))
import qualified Generated.Proto.Data.Value.Type     as Value
import qualified Luna.Target.HS.Control.Error.Data   as Data



class Serializable a b | a -> b where
    serialize :: Mode -> a -> IO (Maybe b)
    toValue :: Mode -> a -> IO (Maybe Value)
    compute :: Mode -> a -> a
    compute = flip const


mkValue :: Key Maybe Value a -> Value.Type -> Maybe a -> Maybe Value
mkValue key keytype = liftM $ \extension -> putExt key (Just extension) $ Value keytype defaultValue


instance Serializable Error ErrorData where
    serialize _ (Error msg) = return . Just $ ErrorData $ fromString msg
    toValue mode a = liftM (mkValue ErrorData.data' Value.Error) $ serialize mode a

instance Serializable () EmptyTupleData where
    serialize _  _ = return . Just $ EmptyTupleData
    toValue mode a = liftM (mkValue EmptyTupleData.data' Value.EmptyTuple) $ serialize mode a

instance Serializable Int IntData where
    serialize _  a = return . Just . IntData . fromIntegral $ a
    toValue mode a = liftM (mkValue IntData.data' Value.Int) $ serialize mode a

instance Serializable Char CharData where
    serialize _  a = return . Just . CharData . fromIntegral . ord $ a
    toValue mode a = liftM (mkValue CharData.data' Value.Char) $ serialize mode a

instance Serializable Bool BoolData where
    serialize _  a = return . Just . BoolData $ a
    toValue mode a = liftM (mkValue BoolData.data' Value.Bool) $ serialize mode a

instance Serializable String StringData where
    serialize _  a = return . Just . StringData $ fromString a
    toValue mode a = liftM (mkValue StringData.data' Value.String) $ serialize mode a

instance Serializable Float FloatData where
    serialize _  a = return . Just . FloatData $ a
    toValue mode a = liftM (mkValue FloatData.data' Value.Float) $ serialize mode a

instance Serializable Double DoubleData.DoubleData where
    serialize _  a = return . Just . DoubleData $ a
    toValue mode a = liftM (mkValue DoubleData.data' Value.Double) $ serialize mode a


-- [PM] : instance below requires UndecidableInstances enabled
instance Serializable a b => Serializable (Data.Safe a) b where
    serialize mode (Data.Safe a) = serialize mode a
    toValue   mode (Data.Safe a) = toValue mode a
    compute   mode (Data.Safe a) = Data.Safe $ compute mode a

-- TODO [PM] Instance for unsafe
--instance Serializable a b => Serializable (Data.Unsafe a) b where
