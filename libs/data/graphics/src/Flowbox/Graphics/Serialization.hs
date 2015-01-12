---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Flowbox.Graphics.Serialization where

import Flowbox.Graphics.Prelude hiding (views)

import           Control.Error.Util
import           Control.Monad
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Array.Sugar as A
import           Data.Array.Accelerate.IO
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Lazy              (fromStrict)
import           Data.Maybe                        (fromMaybe)
import           Data.Set

import           Flowbox.Data.Mode                    (Mode)
import           Flowbox.Data.Serialization           (Serializable (..), mkValue)
import qualified Flowbox.Graphics.Image.Channel       as Chan
import qualified Flowbox.Graphics.Image.Image         as Img
import qualified Flowbox.Graphics.Image.View          as V
import           Flowbox.Graphics.Shader.Sampler      (Sampler, monosampler)
import qualified Flowbox.Math.Matrix                  as M
import qualified Generated.Proto.Data.MatrixData      as MatrixData
import qualified Generated.Proto.Data.MatrixData.Type as MatrixData
import qualified Generated.Proto.Data.Value.Type      as Value
import qualified Generated.Proto.Data.ViewData        as ViewData

import qualified Data.Array.Accelerate.CUDA as CUDA



serializationBackend :: M.Backend                    -- TODO [KL, PM]: Do something with that
serializationBackend = CUDA.run                      --

defaultSampler :: Sampler Double                     -- TODO [MM]: ^ and this too
defaultSampler = monosampler


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

serializeChanFromView :: V.View -> String -> Mode -> IO (Maybe MatrixData.MatrixData)
serializeChanFromView v x mode = case join . hush . V.get v $ x of
    Just c -> serializeChan c mode
    Nothing -> return Nothing
    where serializeChan chan m = case chan of
              Chan.ChannelFloat     _ (Chan.asMatrix -> Chan.MatrixData mat) -> serialize mat m
              Chan.ChannelInt       _ (Chan.asMatrix -> Chan.MatrixData mat) -> serialize mat m
              Chan.ChannelBit       _ (Chan.asMatrix -> Chan.MatrixData mat) -> serialize mat m
              --gen@I.ChannelShader{}                               -> serializeChan (I.compute serializationBackend defaultSampler gen) mode
              --INFO[KM]: the above line is probably obsolete since serialize mat will serialize the matrix, although it might be necessary to update some `compute` functions to handle the computation of the Shaders

instance Serializable V.View ViewData.ViewData where
    serialize v mode = do
        red   <- serializeChanFromView v "rgba.r" mode
        green <- serializeChanFromView v "rgba.g" mode
        blue  <- serializeChanFromView v "rgba.b" mode
        alpha <- serializeChanFromView v "rgba.a" mode
        return $ liftM4 ViewData.ViewData red green blue (Just alpha)

    toValue a mode = liftM (mkValue ViewData.data' Value.View) $ serialize a mode
    compute a _    = V.map (Chan.compute serializationBackend defaultSampler) a

instance Serializable Img.Image ViewData.ViewData where
    serialize (Img.DefaultView view) = serialize $ fromMaybe (error "Can not serialize an empty view!") view
    toValue a mode = liftM (mkValue ViewData.data' Value.View) $ serialize a mode
    compute a _    = Img.map (V.map $ Chan.compute serializationBackend defaultSampler) a
