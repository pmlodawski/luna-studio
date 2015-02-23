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
import           Flowbox.Prelude                     hiding (fromString)
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
    serialize :: a -> Mode -> IO (Maybe b)
    data' :: a -> Key Maybe Value b
    val   :: a -> Value.Type

    toValue :: a -> Mode -> IO (Maybe Value)
    toValue a mode = mkValue (data' a) (val a) <$> serialize a mode


mkValue :: Key Maybe Value a -> Value.Type -> Maybe a -> Maybe Value
mkValue key keytype = liftM $ \extension -> putExt key (Just extension) $ Value keytype defaultValue


instance Serializable Error ErrorData where
    serialize (Error msg) _ = return . Just . ErrorData $ fromString msg
    data' _ = ErrorData.data'
    val   _ = Value.Error

instance Serializable () EmptyTupleData where
    serialize _  _ = return . Just $ EmptyTupleData
    data' _ = EmptyTupleData.data'
    val   _ = Value.EmptyTuple

instance Serializable Int IntData where
    serialize a  _ = return . Just . IntData . fromIntegral $ a
    data' _ = IntData.data'
    val   _ = Value.Int

instance Serializable Char CharData where
    serialize a  _ = return . Just . CharData . fromIntegral . ord $ a
    data' _ = CharData.data'
    val   _ = Value.Char

instance Serializable Bool BoolData where
    serialize a  _ = return . Just . BoolData $ a
    data' _ = BoolData.data'
    val   _ = Value.Bool

instance Serializable String StringData where
    serialize a  _ = return . Just . StringData $ fromString a
    data' _ = StringData.data'
    val   _ = Value.String

instance Serializable Float FloatData where
    serialize a  _ = return . Just . FloatData $ a
    data' _ = FloatData.data'
    val   _ = Value.Float

instance Serializable Double DoubleData.DoubleData where
    serialize a  _ = return . Just . DoubleData $ a
    data' _ = DoubleData.data'
    val   _ = Value.Double


-- [PM] : instance below requires UndecidableInstances enabled
instance Serializable a b => Serializable (Data.Safe a) b where
    serialize (Data.Safe a) mode = serialize a mode
    data'     _ = data' (undefined :: a)
    val       _ = val (undefined :: a)

-- TODO [PM] Instance for unsafe
--instance Serializable a b => Serializable (Data.Unsafe a) b where
