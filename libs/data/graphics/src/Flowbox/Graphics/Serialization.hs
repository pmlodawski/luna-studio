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
import qualified Data.Array.Accelerate                             as A
import qualified Data.Array.Accelerate.Array.Data                  as A
import qualified Data.Array.Accelerate.Array.Sugar                 as A hiding (size)
import           Data.Array.Accelerate.IO
import           Data.ByteString                                   as S (ByteString)
import           Data.ByteString.Lazy                              as L (ByteString, fromStrict)
-- import           Data.Set
import           Data.Sequence                                     (Seq, ViewL (..))
import qualified Data.Sequence                                     as Seq
import qualified Data.Vector.Storable                              as SV
import           Foreign                                           (Storable (..), alloca, castPtr, copyBytes, mallocBytes)
import           System.IO.Unsafe                                  (unsafePerformIO)
import           Text.ProtocolBuffers.Basic                        (uFromString, uToString)

import qualified Flowbox.Data.Mode                                 as Mode (Mode (..))
import           Flowbox.Data.Serialization                        (Serializable (..))
import qualified Flowbox.Graphics.Image.Channel                    as Chan
import qualified Flowbox.Graphics.Image.Image                      as Img
import qualified Flowbox.Graphics.Image.View                       as V
import           Flowbox.Graphics.Prelude                          hiding (mapM, views)
import           Flowbox.Graphics.Shader.Sampler                   (Sampler, monosampler)
import qualified Flowbox.Math.Matrix                               as M
import qualified Generated.Proto.Data.Channel                      as ProtoChan
import qualified Generated.Proto.Data.ChannelData                  as ChanData
import qualified Generated.Proto.Data.ChannelDescription           as ChanDesc
import qualified Generated.Proto.Data.ChannelDescription.ChannelID as ChID
import qualified Generated.Proto.Data.MatrixData                   as MatrixData
import qualified Generated.Proto.Data.MatrixData.Tag               as MatrixData
import qualified Generated.Proto.Data.MatrixData.Type              as MatrixData
import qualified Generated.Proto.Data.SValue.Type                  as SValue
import           Luna.Interpreter.Runtime.Serialization

import qualified Data.Array.Accelerate.CUDA                        as CUDA



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


data BinaryMatrix = BinaryMatrix { container :: BinaryData
                                 , width     :: Int
                                 , height    :: Int
                                 , matType   :: MatrixData.Type
                                 }

data BinaryData = BS L.ByteString
                | Pointer A.Word64

data MatrixMode = PtrMode | BSMode


serializeMatrix :: forall a. (Storable a, MatType a, M.Elt a, ByteStrings (A.EltRepr a) ~ ((), S.ByteString),
                Vectors (A.EltRepr a) ~ ((), SV.Vector a))
                => MatrixMode -> M.Matrix2 a -> IO MatrixData.MatrixData
serializeMatrix BSMode (M.Raw arr) = do
    ((), mat) <- toByteString arr
    let M.Z M.:. h M.:. w = A.arrayShape arr

    return $ MatrixData.MatrixData (Just $ fromStrict mat) Nothing        -- data
                                   (fromIntegral w)              -- width
                                   (fromIntegral h)              -- height
                                   (getMatType (undefined :: a)) -- precision
                                   MatrixData.BINARY
serializeMatrix PtrMode (M.Raw arr) = do
    let ((), vec) = toVectors arr
        M.Z M.:. h A.:. w = A.arrayShape arr
        lengthInBytes = h * w * sizeOf (undefined :: a)
        ptrToWord64 p = unsafePerformIO $ alloca $ \a -> do
            poke a p
            peek $ castPtr a
    mem <- mallocBytes lengthInBytes
    SV.unsafeWith vec $ \ptr -> copyBytes mem ptr lengthInBytes
    return $ MatrixData.MatrixData Nothing (Just $ ptrToWord64 mem)
                                  (fromIntegral w)
                                  (fromIntegral h)
                                  (getMatType (undefined :: a))
                                  MatrixData.POINTER
serializeMatrix _ _ = error "channel wasn't computed"

-- serializeChanFromView :: V.View -> String -> Mode.Mode -> IO (Maybe MatrixData.MatrixData)
-- serializeChanFromView v x mode = case join . hush . V.get v $ x of
--     Just c -> serializeChan c mode
--     Nothing -> return Nothing
--     where serializeChan chan m = case chan of
--               Chan.ChannelFloat     _ (Chan.asMatrixData -> Chan.MatrixData mat) -> serialize mat m
--               Chan.ChannelInt       _ (Chan.asMatrixData -> Chan.MatrixData mat) -> serialize mat m

pattern Mode a as <- Mode.Mode (Seq.viewl -> a :< (Mode.Mode -> as))
pattern EmptyMode <- Mode.Mode (Seq.viewl -> EmptyL)

-- batchCompute' :: [Chan.Channel] -> Maybe (M.Matrix2 (Double, Double, Double, Double))
-- batchCompute' [ChanF nr r, ChanF ng g, ChanF nb b, ChanF na a] = Just $ M.compute serializationBackend $ M.zip4 r g b a
-- batchCompute' _ = Nothing

-- serialize' :: forall a. (A.Elt (a, a, a, a)
--            , ByteStrings (A.EltRepr (a, a, a, a)) ~ (((((), S.ByteString), S.ByteString), S.ByteString), S.ByteString)
--            , MatType a)
--            => M.Matrix2 (a, a, a, a) -> Mode.Mode -> IO (Maybe ViewData.ViewData)
-- serialize' (M.Raw arr) _ = do
--     (((((), r), g), b), a) <- toByteString arr
--     let M.Z M.:. h M.:. w = A.arrayShape arr
--         toData x = MatrixData.MatrixData (fromStrict x) (fromIntegral w) (fromIntegral h) (getMatType (undefined :: a))

--     return . Just $ ViewData.ViewData (toData r) (toData g) (toData b) (Just $ toData a)

-- instance Serializable V.View ViewData.ViewData where
--     serialize v mode = do
--         let channels = fromMaybe (error "Not found channels in image") $ get ["rgba.r", "rgba.g", "rgba.b", "rgba.a"] v
--         case batchCompute' channels of
--             Just mat -> undefined
--             _        -> do
--                 red   <- serializeChanFromView v "rgba.r" mode
--                 green <- serializeChanFromView v "rgba.g" mode
--                 blue  <- serializeChanFromView v "rgba.b" mode
--                 alpha <- serializeChanFromView v "rgba.a" mode
--                 return $ liftM4 ViewData.ViewData red green blue (Just alpha)

--     data' _ = ViewData.data'
--     val   _ = SValue.View

get :: [String] -> V.View -> Maybe [Chan.Channel]
get names view = mapM (join . hush . V.get view) names

-- compute' :: ChanDesc.ChannelDescription -> V.View -> V.View
-- compute' (ChanDesc.ChannelDescription _type chanID name) v = case channels chanID name v of
--     Just cs -> foldr V.append v $ batchCompute cs
--     _       -> V.map (Chan.compute serializationBackend) v

-- channels ::
channels chanID name v = case chanID of
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

chan :: Chan.Name -> M.Matrix2 Float -> Chan.Channel
chan n m = Chan.ChannelFloat n $ Chan.MatrixData m

batchCompute :: [Chan.Channel] -> [Chan.Channel]
batchCompute [ChanF nr r, ChanF ng g, ChanF nb b, ChanF na a] = [chan nr r', chan ng g', chan nb b', chan na a']
    where (r', g', b', a') = over each M.Raw $ unzip4Raw $ (\(M.Raw a) -> a) $ M.compute serializationBackend $ M.zip4 r g b a
batchCompute (ChanF n x : xs) = chan n (M.compute serializationBackend x) : batchCompute xs
batchCompute [] = []

instance Serializable Img.Image ChanData.ChannelData where
    serialize (Img.ForceView view) mode = (Just . ChanData.ChannelData) <$> serialize0 Seq.empty view mode
        where serialize0 :: Seq ProtoChan.Channel -> V.View -> Mode.Mode -> IO (Seq ProtoChan.Channel)
              serialize0 acc _ EmptyMode = return acc
              serialize0 acc v (Mode (ChanDesc.ChannelDescription _type chanID name) as) =
                  newChans >>= \n -> serialize0 (acc Seq.>< n) v as
                  where newChans = do
                            let chans = channels chanID name v
                            case chans of
                                Just x -> do
                                    let computed = batchCompute x
                                        chandescs = map mkChanDesc computed
                                    serialized <- mapM (serializeMatrix BSMode . unpackMatrix) computed
                                    return $ Seq.fromList $ zipWith ProtoChan.Channel chandescs serialized
                                _      -> error "channels not found"

    serialize _ _ = return Nothing

    data' _ = ChanData.data'
    val   _ = SValue.ChannelData

mkChanDesc :: Chan.Channel -> ChanDesc.ChannelDescription
mkChanDesc (ChanF n a) = ChanDesc.ChannelDescription (Just MatrixData.DOUBLE) (nameToID n) (Just $ uFromString n)
    where nameToID "rgba.r" = ChID.Red
          nameToID "rgba.g" = ChID.Green
          nameToID "rgba.b" = ChID.Blue
          nameToID "rgba.a" = ChID.Alpha
          nameToID _        = ChID.Other

unpackMatrix :: Chan.Channel -> M.Matrix2 Float
unpackMatrix (ChanF _ a) = a

unzip4Raw :: (A.Shape sh, A.Elt (a, b, c, d), A.Elt a, A.Elt b, A.Elt c, A.Elt d,
              A.EltRepr a ~ ((), A.EltRepr' a),
              A.EltRepr b ~ ((), A.EltRepr' b),
              A.EltRepr c ~ ((), A.EltRepr' c),
              A.EltRepr d ~ ((), A.EltRepr' d)
              ) => A.Array sh (a, b, c, d) -> (A.Array sh a, A.Array sh b, A.Array sh c, A.Array sh d)
unzip4Raw (A.Array sh e) = case e of
  A.AD_Pair x1 d -> case x1 of
    A.AD_Pair x2 c -> case x2 of
      A.AD_Pair x3 b -> case x3 of
        A.AD_Pair A.AD_Unit a -> (A.Array sh (A.AD_Pair A.AD_Unit a), A.Array sh (A.AD_Pair A.AD_Unit b),
                                  A.Array sh (A.AD_Pair A.AD_Unit c), A.Array sh (A.AD_Pair A.AD_Unit d))
