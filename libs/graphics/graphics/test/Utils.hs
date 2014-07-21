---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module Utils where

import           Flowbox.Prelude                      as P
import           Flowbox.Math.Matrix                  as M
import           Flowbox.Graphics.Composition.Generators.Rasterizer
import           Flowbox.Graphics.Composition.Generators.Sampler
import           Flowbox.Graphics.Image.IO.ImageMagick (loadImage, saveImage)
import           Flowbox.Graphics.Utils

import qualified Codec.Picture.Png          as Juicy
import qualified Codec.Picture.Types        as Juicy
import qualified Data.Array.Accelerate      as A
import           Data.Array.Accelerate.IO
import           Data.Array.Accelerate.CUDA
import qualified Data.Vector.Storable       as SV
import           Math.Space.Space



type IOLoadBackend a = FilePath -> IO (Either a (A.Array A.DIM2 RGBA32))
type IOSaveBackend   = FilePath -> A.Array A.DIM2 RGBA32 -> IO ()

testFunction :: (A.Exp Float -> A.Exp Float)
             -> FilePath
             -> FilePath
             -> IO ()
testFunction f input output = do
    img <- map4 (fromMatrix A.Wrap) <$> testLoadRGBA' input
    let (r', g', b', a') = img & each %~ (rasterizer (Grid 512 512) . (fmap f) . monosampler)
    testSaveRGBA' output r' g' b' a'

testLoadRGBA :: (Show a, Elt b, IsFloating b) => IOLoadBackend a -> FilePath -> IO (Matrix2 b, Matrix2 b, Matrix2 b, Matrix2 b)
testLoadRGBA backend filename = do
    file <- backend filename
    case file of
        Right mat -> return $ M.unzip4 $ M.map (convert . unpackRGBA32) (Raw mat)
        Left e -> error $ "Unable to load file: " P.++ show e
    where convert t = let (r, g, b, a) = A.unlift t :: (Exp A.Word8, Exp A.Word8, Exp A.Word8, Exp A.Word8)
                      in A.lift (A.fromIntegral r / 255, A.fromIntegral g / 255, A.fromIntegral b / 255, A.fromIntegral a / 255)

testLoadRGBA' :: (Elt b, IsFloating b) => FilePath -> IO (Matrix2 b, Matrix2 b, Matrix2 b, Matrix2 b)
testLoadRGBA' = testLoadRGBA loadImage


testSaveRGBA :: (Elt a, IsFloating a) => IOSaveBackend -> FilePath -> Matrix2 a -> Matrix2 a -> Matrix2 a -> Matrix2 a -> IO ()
testSaveRGBA backend filename r g b a = backend filename $ compute' run $ M.map packRGBA32 $ zip4 (conv r) (conv g) (conv b) (conv a)
    where conv = M.map (A.truncate . (* 255.0) . clamp' 0 1)

testSaveRGBA' :: (Elt a, IsFloating a) => FilePath -> Matrix2 a -> Matrix2 a -> Matrix2 a -> Matrix2 a -> IO ()
testSaveRGBA' = testSaveRGBA saveImage

testSaveRGBA'' :: (Elt a, IsFloating a) => FilePath -> Matrix2 a -> Matrix2 a -> Matrix2 a -> Matrix2 a -> IO ()
testSaveRGBA'' = testSaveRGBA saveImageJuicy

saveImageJuicy :: IOSaveBackend
saveImageJuicy file matrix = do
    let ((), vec) = toVectors matrix
        A.Z A.:. h A.:. w = A.arrayShape matrix
    Juicy.writePng file $ (Juicy.Image w h (SV.unsafeCast vec) :: Juicy.Image Juicy.PixelRGBA8) 


testSaveChan :: (Elt a, IsFloating a) => IOSaveBackend -> FilePath -> Matrix2 a -> IO ()
testSaveChan backend filename pre = backend filename $ compute' run $ M.map rgba32OfFloat output
    where output = M.map conv pre
          conv (clamp' 0 1 -> x) = A.lift (x, x, x, x)

testSaveChan' :: (Elt a, IsFloating a) => FilePath -> Matrix2 a -> IO ()
testSaveChan' = testSaveChan saveImage

map4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
map4 f (a, b, c, d) = (f a, f b, f c, f d)