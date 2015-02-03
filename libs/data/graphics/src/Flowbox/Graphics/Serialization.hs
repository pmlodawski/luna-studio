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
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Flowbox.Graphics.Serialization where


import           Control.Error.Util
import           Control.Monad
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Array.Sugar as A
import           Data.Array.Accelerate.IO
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Lazy              (fromStrict)
import           Data.Maybe                        (fromMaybe)
-- import           Data.Set
import           Data.Sequence                     (ViewL(..))
import qualified Data.Sequence                     as Seq
import           Text.ProtocolBuffers.Basic        (uToString)

import qualified Flowbox.Data.Mode                    as Mode (Mode(..))
import           Flowbox.Data.Serialization           (Serializable (..), mkValue)
import qualified Flowbox.Graphics.Image.Channel       as Chan
import qualified Flowbox.Graphics.Image.Image         as Img
import qualified Flowbox.Graphics.Image.View          as V
import           Flowbox.Graphics.Prelude             hiding (mapM, views)
import           Flowbox.Graphics.Shader.Sampler      (Sampler, monosampler)
import           Flowbox.Graphics.Utils.Utils         (fstQuad)
import qualified Flowbox.Math.Matrix                  as M
import qualified Generated.Proto.Data.ChannelDescription as ChanDesc
import qualified Generated.Proto.Data.ChannelDescription.ChannelID as ChID
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

serializeChanFromView :: V.View -> String -> Mode.Mode -> IO (Maybe MatrixData.MatrixData)
serializeChanFromView v x mode = case join . hush . V.get v $ x of
    Just c -> serializeChan c mode
    Nothing -> return Nothing
    where serializeChan chan m = case chan of
              Chan.ChannelFloat     _ (Chan.asMatrixData -> Chan.MatrixData mat) -> serialize mat m
              Chan.ChannelInt       _ (Chan.asMatrixData -> Chan.MatrixData mat) -> serialize mat m

pattern Mode a as <- Mode.Mode (Seq.viewl -> a :< as)
pattern EmptyMode <- Mode.Mode (Seq.viewl -> EmptyL)

batchCompute' :: [Chan.Channel] -> Maybe (M.Matrix2 (Double, Double, Double, Double))
batchCompute' [ChanF nr r, ChanF ng g, ChanF nb b, ChanF na a] = Just $ M.compute serializationBackend $ M.zip4 r g b a
batchCompute' _ = Nothing

serialize' :: forall a. (A.Elt (a, a, a, a)
           , ByteStrings (A.EltRepr (a, a, a, a)) ~ (((((), ByteString), ByteString), ByteString), ByteString)
           , MatType a)
           => M.Matrix2 (a, a, a, a) -> Mode.Mode -> IO (Maybe ViewData.ViewData)
serialize' (M.Raw arr) _ = do
    (((((), r), g), b), a) <- toByteString arr
    let M.Z M.:. h M.:. w = A.arrayShape arr
        toData x = MatrixData.MatrixData (fromStrict x) (fromIntegral w) (fromIntegral h) (getMatType (undefined :: a))

    return . Just $ ViewData.ViewData (toData r) (toData g) (toData b) (Just $ toData a)

instance Serializable V.View ViewData.ViewData where
    serialize v mode = do
        let channels = get ["rgba.r", "rgba.g", "rgba.b", "rgba.a"] v
        case channels of
            Just cs -> case batchCompute' cs of
                Just mat -> serialize' mat mode
                _        -> do
                    red   <- serializeChanFromView v "rgba.r" mode
                    green <- serializeChanFromView v "rgba.g" mode
                    blue  <- serializeChanFromView v "rgba.b" mode
                    alpha <- serializeChanFromView v "rgba.a" mode
                    return $ liftM4 ViewData.ViewData red green blue (Just alpha)
            _       -> return Nothing

    toValue a mode = liftM (mkValue ViewData.data' Value.View) $ serialize a mode

    compute a EmptyMode  = a
    compute a (Mode m ms) = compute (compute' m a) $ Mode.Mode ms

get :: [String] -> V.View -> Maybe [Chan.Channel]
get names view = mapM (join . hush . V.get view) names

compute' :: ChanDesc.ChannelDescription -> V.View -> V.View
compute' (ChanDesc.ChannelDescription _type chanID name) v = case channels of
    Just cs -> foldr V.append v $ batchCompute cs
    _       -> V.map (Chan.compute serializationBackend) v
    where channels = case chanID of
              ChID.RGBA  -> get ["rgba.r", "rgba.g", "rgba.b", "rgba.a"] v
              ChID.Other -> case name of
                  Just n -> get [uToString n] v
                  _      -> Nothing
              ChID.Red   -> get ["rgba.r"] v
              ChID.Green -> get ["rgba.g"] v
              ChID.Blue  -> get ["rgba.b"] v
              ChID.Alpha -> get ["rgba.a"] v

pattern ChanF n mat <- Chan.ChannelFloat n (Chan.asMatrixData -> Chan.MatrixData mat)
pattern ChanI n mat <- Chan.ChannelInt   n (Chan.asMatrixData -> Chan.MatrixData mat)

batchCompute :: [Chan.Channel] -> [Chan.Channel]
batchCompute = id

instance Serializable Img.Image ViewData.ViewData where
    serialize (Img.DefaultView view) = serialize $ fromMaybe (error "Can not serialize an empty view!") view
    toValue a mode = liftM (mkValue ViewData.data' Value.View) $ serialize a mode
    compute a _    = Img.map (V.map $ Chan.compute serializationBackend) a
