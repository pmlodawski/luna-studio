---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Flowbox.Graphics.Serialization where

import Flowbox.Graphics.Prelude

import           Control.Error.Util
import           Control.Monad
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Array.Sugar as A
import           Data.Array.Accelerate.IO
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Lazy              (fromStrict)

import           Flowbox.Data.Serialization           (Serializable (..), mkValue)
import qualified Flowbox.Graphics.Image.Channel       as I
import qualified Flowbox.Graphics.Image.View          as I
import qualified Flowbox.Math.Matrix                  as M
import qualified Generated.Proto.Data.MatrixData      as MatrixData
import qualified Generated.Proto.Data.MatrixData.Type as MatrixData
import qualified Generated.Proto.Data.Value.Type      as Value
import qualified Generated.Proto.Data.ViewData        as ViewData

import qualified Data.Array.Accelerate.CUDA as CUDA
serializationBackend :: M.Backend                    -- TODO [KL, PM]: Do something with that
serializationBackend = CUDA.run                      --



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
