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

module Flowbox.Data.Serialization where

import Control.Monad
import Data.Char                        (ord)
import Text.ProtocolBuffers.Basic       (defaultValue)
import Text.ProtocolBuffers.Extensions
import Text.ProtocolBuffers.Identifiers

import           Flowbox.Prelude
import qualified Generated.Proto.Data.BoolData        as BoolData
import qualified Generated.Proto.Data.CharData        as CharData
import qualified Generated.Proto.Data.DoubleData      as DoubleData
import qualified Generated.Proto.Data.FloatData       as FloatData
import qualified Generated.Proto.Data.IntData         as IntData
import qualified Generated.Proto.Data.MatrixData.Type as MatrixData
import qualified Generated.Proto.Data.StringData      as StringData
import qualified Generated.Proto.Data.Value           as Value
import qualified Generated.Proto.Data.Value.Type      as Value



class Serializable a b | a -> b where
    serialize :: a -> IO (Maybe b)
    toValue :: a -> IO (Maybe Value.Value)
    compute :: a -> a
    compute = id


mkValue :: Key Maybe Value.Value a -> Value.Type -> Maybe a -> Maybe Value.Value
mkValue key keytype = liftM $ \extension -> putExt key (Just extension) $ Value.Value keytype defaultValue



instance Serializable Int IntData.IntData where
    serialize a = return . Just . IntData.IntData . fromIntegral $ a
    toValue a = liftM (mkValue IntData.data' Value.Int) $ serialize a

instance Serializable Char CharData.CharData where
    serialize a = return . Just . CharData.CharData . fromIntegral . ord $ a
    toValue a = liftM (mkValue CharData.data' Value.Char) $ serialize a

instance Serializable Bool BoolData.BoolData where
    serialize a = return . Just . BoolData.BoolData $ a
    toValue a = liftM (mkValue BoolData.data' Value.Bool) $ serialize a

instance Serializable String StringData.StringData where
    serialize a = return . Just . StringData.StringData $ fromString a
    toValue a = liftM (mkValue StringData.data' Value.String) $ serialize a

instance Serializable Float FloatData.FloatData where
    serialize a = return . Just . FloatData.FloatData $ a
    toValue a = liftM (mkValue FloatData.data' Value.Float) $ serialize a

instance Serializable Double DoubleData.DoubleData where
    serialize a = return . Just . DoubleData.DoubleData $ a
    toValue a = liftM (mkValue DoubleData.data' Value.Double) $ serialize a

