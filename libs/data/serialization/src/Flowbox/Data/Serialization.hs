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

module Flowbox.Data.Serialization (
    module Flowbox.Data.Serialization,
    Mode,
    SValue,
) where

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
import           Generated.Proto.Data.SValue         (SValue (SValue))
import qualified Generated.Proto.Data.SValue.Type    as SValue

class Serializable a b | a -> b where
    serialize :: a -> Mode -> IO (Maybe b)
    data' :: a -> Key Maybe SValue b
    val   :: a -> SValue.Type

    toValue :: a -> Mode -> IO (Maybe SValue)
    toValue a mode = mkValue (data' a) (val a) <$> serialize a mode


mkValue :: Key Maybe SValue a -> SValue.Type -> Maybe a -> Maybe SValue
mkValue key keytype = liftM $ \extension -> putExt key (Just extension) $ SValue keytype defaultValue


instance Serializable Error ErrorData where
    serialize (Error msg) _ = return . Just . ErrorData $ fromString msg
    data' _ = ErrorData.data'
    val   _ = SValue.Error

instance Serializable () EmptyTupleData where
    serialize _  _ = return . Just $ EmptyTupleData
    data' _ = EmptyTupleData.data'
    val   _ = SValue.EmptyTuple

instance Serializable Int IntData where
    serialize a  _ = return . Just . IntData . fromIntegral $ a
    data' _ = IntData.data'
    val   _ = SValue.Int

instance Serializable Char CharData where
    serialize a  _ = return . Just . CharData . fromIntegral . ord $ a
    data' _ = CharData.data'
    val   _ = SValue.Char

instance Serializable Bool BoolData where
    serialize a  _ = return . Just . BoolData $ a
    data' _ = BoolData.data'
    val   _ = SValue.Bool

instance Serializable String StringData where
    serialize a  _ = return . Just . StringData $ fromString a
    data' _ = StringData.data'
    val   _ = SValue.String

instance Serializable Float FloatData where
    serialize a  _ = return . Just . FloatData $ a
    data' _ = FloatData.data'
    val   _ = SValue.Float

instance Serializable Double DoubleData.DoubleData where
    serialize a  _ = return . Just . DoubleData $ a
    data' _ = DoubleData.data'
    val   _ = SValue.Double

computeValue :: Serializable a b => a -> Mode -> IO (Maybe SValue)
computeValue = toValue
