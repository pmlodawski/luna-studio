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
import           Generated.Proto.Data.BoolData   (BoolData (BoolData))
import qualified Generated.Proto.Data.BoolData   as BoolData
import           Generated.Proto.Data.CharData   (CharData (CharData))
import qualified Generated.Proto.Data.CharData   as CharData
import           Generated.Proto.Data.DoubleData (DoubleData (DoubleData))
import qualified Generated.Proto.Data.DoubleData as DoubleData
import           Generated.Proto.Data.FloatData  (FloatData (FloatData))
import qualified Generated.Proto.Data.FloatData  as FloatData
import           Generated.Proto.Data.IntData    (IntData (IntData))
import qualified Generated.Proto.Data.IntData    as IntData
import           Generated.Proto.Data.StringData (StringData (StringData))
import qualified Generated.Proto.Data.StringData as StringData
import           Generated.Proto.Data.Value      (Value (Value))
import qualified Generated.Proto.Data.Value.Type as Value



class Serializable a b | a -> b where
    serialize :: a -> IO (Maybe b)
    toValue :: a -> IO (Maybe Value)
    compute :: a -> a
    compute = id


mkValue :: Key Maybe Value a -> Value.Type -> Maybe a -> Maybe Value
mkValue key keytype = liftM $ \extension -> putExt key (Just extension) $ Value keytype defaultValue


instance Serializable Int IntData where
    serialize a = return . Just . IntData . fromIntegral $ a
    toValue a = liftM (mkValue IntData.data' Value.Int) $ serialize a

instance Serializable Char CharData where
    serialize a = return . Just . CharData . fromIntegral . ord $ a
    toValue a = liftM (mkValue CharData.data' Value.Char) $ serialize a

instance Serializable Bool BoolData where
    serialize a = return . Just . BoolData $ a
    toValue a = liftM (mkValue BoolData.data' Value.Bool) $ serialize a

instance Serializable String StringData where
    serialize a = return . Just . StringData $ fromString a
    toValue a = liftM (mkValue StringData.data' Value.String) $ serialize a

instance Serializable Float FloatData where
    serialize a = return . Just . FloatData $ a
    toValue a = liftM (mkValue FloatData.data' Value.Float) $ serialize a

instance Serializable Double DoubleData.DoubleData where
    serialize a = return . Just . DoubleData $ a
    toValue a = liftM (mkValue DoubleData.data' Value.Double) $ serialize a

