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

import qualified Generated.Proto.Graphics.IntData              as Proto
import qualified Generated.Proto.Graphics.CharData             as Proto
import qualified Generated.Proto.Graphics.BoolData             as Proto
import qualified Generated.Proto.Graphics.StringData           as Proto
import qualified Generated.Proto.Graphics.FloatData            as Proto
import qualified Generated.Proto.Graphics.DoubleData           as Proto
import qualified Generated.Proto.Graphics.MatrixData           as Proto
import qualified Generated.Proto.Graphics.MatrixData.Type      as Proto
import qualified Generated.Proto.Graphics.ImageData            as Proto

import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Array.Sugar as A
import           Data.ByteString                   (ByteString(..))
import           Data.ByteString.Lazy              (fromStrict)
import           Data.Char                         (ord)
import           Data.Array.Accelerate.IO
import           Text.ProtocolBuffers.Identifiers
import           Control.Error.Util
import           Control.Monad



class Serializable a b | a -> b where
    serialize :: a -> IO (Maybe b)

class MatType a where
    getMatType :: a -> Proto.Type

instance MatType Int where
    getMatType _ = Proto.INT

instance MatType Float where
    getMatType _ = Proto.FLOAT

instance MatType Double where
    getMatType _ = Proto.DOUBLE

instance MatType Bool where
    getMatType _ = Proto.BOOL

instance Serializable Int Proto.IntData where
    serialize a = return . Just . Proto.IntData . fromIntegral $ a

instance Serializable Char Proto.CharData where
    serialize a = return . Just . Proto.CharData . fromIntegral . ord $ a

instance Serializable Bool Proto.BoolData where
    serialize a = return . Just . Proto.BoolData $ a

instance Serializable String Proto.StringData where
    serialize a = return . Just . Proto.StringData $ fromString a

instance Serializable Float Proto.FloatData where
    serialize a = return . Just . Proto.FloatData $ a

instance Serializable Double Proto.DoubleData where
    serialize a = return . Just . Proto.DoubleData $ a

instance ( ByteStrings (A.EltRepr a) ~ ((), ByteString)
         , A.Elt a, MatType a
         ) => Serializable (M.Matrix2 a) Proto.MatrixData where
    serialize (M.Raw arr) = do
        ((), mat) <- toByteString arr
        let M.Z M.:. h M.:. w = A.arrayShape arr

        return . Just $ Proto.MatrixData (fromStrict mat)              -- data
                                         (fromIntegral w)              -- width
                                         (fromIntegral h)              -- height
                                         (getMatType (undefined :: a)) -- precision
    serialize _ = return Nothing

serializeChan :: I.View v => v -> String -> IO (Maybe Proto.MatrixData)
serializeChan v x = case join . hush . (I.get v) $ x of
    Just c -> case c of
        I.ChannelFloat _ (I.FlatData mat) -> serialize mat
        I.ChannelInt   _ (I.FlatData mat) -> serialize mat
        I.ChannelBit   _ (I.FlatData mat) -> serialize mat
    Nothing -> return Nothing

instance Serializable I.RGBA Proto.ImageData where
    serialize v = do
        red   <- serializeChan v "rgba.r"
        green <- serializeChan v "rgba.g"
        blue  <- serializeChan v "rgba.b"
        alpha <- serializeChan v "rgba.a"
        return $ liftM4 Proto.ImageData red green blue (Just alpha)

instance Serializable I.RGB Proto.ImageData where
    serialize v = do
        red   <- serializeChan v "rgba.r"
        green <- serializeChan v "rgba.g"
        blue  <- serializeChan v "rgba.b"
        return $ liftM4 Proto.ImageData red green blue Nothing
