---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Flowbox.Graphics.Serialization where

import Flowbox.Graphics.Prelude

import           Control.Error.Util
import           Control.Monad
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Array.Sugar as A
import           Data.Array.Accelerate.IO
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Lazy              (fromStrict)
import           Data.Map.Lazy

import           Flowbox.Data.Mode                    (Mode)
import           Flowbox.Data.Serialization           (Serializable (..), mkValue)
import qualified Flowbox.Graphics.Image.Channel       as I
import qualified Flowbox.Graphics.Image.Image         as Img
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
    serialize (M.Raw arr) _ = do
        ((), mat) <- toByteString arr
        let M.Z M.:. h M.:. w = A.arrayShape arr

        return . Just $ MatrixData.MatrixData (fromStrict mat)              -- data
                                              (fromIntegral w)              -- width
                                              (fromIntegral h)              -- height
                                              (getMatType (undefined :: a)) -- precision
    serialize _ _ = return Nothing

    toValue a mode = liftM (mkValue MatrixData.data' Value.Matrix) $ serialize a mode

    compute a _ = M.compute serializationBackend a

serializeChan :: I.View v => v -> String -> Mode -> IO (Maybe MatrixData.MatrixData)
serializeChan v x mode = case join . hush . I.get v $ x of
    Just c -> case c of
        I.ChannelFloat _ (I.FlatData mat) -> serialize mat mode
        I.ChannelInt   _ (I.FlatData mat) -> serialize mat mode
        I.ChannelBit   _ (I.FlatData mat) -> serialize mat mode
    Nothing -> return Nothing

instance Serializable I.RGBA ViewData.ViewData where
    serialize v mode = do
        red   <- serializeChan v "r" mode -- TODO [KM]: Change to rgba.r etc..
        green <- serializeChan v "g" mode
        blue  <- serializeChan v "b" mode
        alpha <- serializeChan v "a" mode
        return $ liftM4 ViewData.ViewData red green blue (Just alpha)

    toValue a mode = liftM (mkValue ViewData.data' Value.View) $ serialize a mode
    compute a _    = I.map (I.compute serializationBackend) a

instance Serializable I.RGB ViewData.ViewData where
    serialize v mode = do
        red   <- serializeChan v "r" mode
        green <- serializeChan v "g" mode
        blue  <- serializeChan v "b" mode
        return $ liftM4 ViewData.ViewData red green blue Nothing

    toValue a mode = liftM (mkValue ViewData.data' Value.View) $ serialize a mode
    compute a _    = I.map (I.compute serializationBackend) a

instance Serializable (Img.Image I.RGB) ViewData.ViewData where
    serialize (Img.Image views _) = serialize (snd $ findMin views)
    toValue a mode = liftM (mkValue ViewData.data' Value.View) $ serialize a mode
    compute a _    = Img.map (I.map $ I.compute serializationBackend) a

instance Serializable (Img.Image I.RGBA) ViewData.ViewData where
    serialize (Img.Image views _) = serialize (snd $ findMin views)
    toValue a mode = liftM (mkValue ViewData.data' Value.View) $ serialize a mode
    compute a _    = Img.map (I.map $ I.compute serializationBackend) a
