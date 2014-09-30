---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Flowbox.Graphics.Serialization where

import Flowbox.Graphics.Prelude

import qualified Flowbox.Math.Matrix              as M
import qualified Flowbox.Graphics.Image.View      as I
import qualified Flowbox.Graphics.Image.Channel   as I

import qualified Generated.Proto.Graphics.Value           as Value
import qualified Generated.Proto.Graphics.Value.Type      as Value
import qualified Generated.Proto.Graphics.IntData         as IntData
import qualified Generated.Proto.Graphics.CharData        as CharData
import qualified Generated.Proto.Graphics.BoolData        as BoolData
import qualified Generated.Proto.Graphics.StringData      as StringData
import qualified Generated.Proto.Graphics.FloatData       as FloatData
import qualified Generated.Proto.Graphics.DoubleData      as DoubleData
import qualified Generated.Proto.Graphics.MatrixData      as MatrixData
import qualified Generated.Proto.Graphics.MatrixData.Type as MatrixData
import qualified Generated.Proto.Graphics.ViewData        as ViewData

import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Array.Sugar as A
import           Data.Array.Accelerate.IO
import           Text.ProtocolBuffers.Identifiers
import           Text.ProtocolBuffers.Extensions
import           Text.ProtocolBuffers.Basic        (defaultValue)
import           Data.ByteString.Lazy              (fromStrict)
import           Data.ByteString                   (ByteString(..))
import           Data.Char                         (ord)
import           Control.Error.Util
import           Control.Monad

import qualified Flowbox.Math.Matrix as M

import qualified Data.Array.Accelerate.CUDA as CUDA  --
serializationBackend :: M.Backend                    -- TODO [KL, PM]: Do something with that
serializationBackend = CUDA.run                      --



class Serializable a b | a -> b where
    serialize :: a -> IO (Maybe b)
    toValue :: a -> IO (Maybe Value.Value)
    compute :: a -> a
    compute = id

mkValue :: Key Maybe Value.Value a -> Value.Type -> Maybe a -> Maybe Value.Value
mkValue key keytype = liftM $ \extension -> putExt key (Just extension) $ Value.Value keytype defaultValue

class MatType a where
    getMatType :: a -> MatrixData.Type

instance MatType Int where
    getMatType _ = MatrixData.INT

instance MatType Float where
    getMatType _ = MatrixData.FLOAT

instance MatType Double where
    getMatType _ = MatrixData.DOUBLE

instance MatType Bool where
    getMatType _ = MatrixData.BOOL

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

instance ( ByteStrings (A.EltRepr a) ~ ((), ByteString)
         , A.Elt a, MatType a
         ) => Serializable (M.Matrix2 a) MatrixData.MatrixData where
    serialize (M.Raw arr) = do
        ((), mat) <- toByteString arr
        let M.Z M.:. h M.:. w = A.arrayShape arr

        return . Just $ MatrixData.MatrixData (fromStrict mat)              -- data
                                              (fromIntegral w)              -- width
                                              (fromIntegral h)              -- height
                                              (getMatType (undefined :: a)) -- precision
    serialize _ = return Nothing

    toValue a = liftM (mkValue MatrixData.data' Value.Matrix) $ serialize a

    compute = M.compute serializationBackend

serializeChan :: I.View v => v -> String -> IO (Maybe MatrixData.MatrixData)
serializeChan v x = case join . hush . I.get v $ x of
    Just c -> case c of
        I.ChannelFloat _ (I.FlatData mat) -> serialize mat
        I.ChannelInt   _ (I.FlatData mat) -> serialize mat
        I.ChannelBit   _ (I.FlatData mat) -> serialize mat
    Nothing -> return Nothing

instance Serializable I.RGBA ViewData.ViewData where
    serialize v = do
        red   <- serializeChan v "r" -- TODO [KM]: Change to rgba.r etc..
        green <- serializeChan v "g"
        blue  <- serializeChan v "b"
        alpha <- serializeChan v "a"
        return $ liftM4 ViewData.ViewData red green blue (Just alpha)

    toValue a = liftM (mkValue ViewData.data' Value.View) $ serialize a
    compute = I.map $ I.compute serializationBackend

instance Serializable I.RGB ViewData.ViewData where
    serialize v = do
        red   <- serializeChan v "rgb.r"
        green <- serializeChan v "rgb.g"
        blue  <- serializeChan v "rgb.b"
        return $ liftM4 ViewData.ViewData red green blue Nothing

    toValue a = liftM (mkValue ViewData.data' Value.View) $ serialize a
    compute = I.map $ I.compute serializationBackend
