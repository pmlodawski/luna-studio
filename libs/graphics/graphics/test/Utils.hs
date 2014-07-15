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
import           Flowbox.Graphics.Utils

import qualified Data.Array.Accelerate                as A
import           Data.Array.Accelerate.IO
import           Data.Array.Accelerate.CUDA

testLoadRGBA :: (Elt a, IsFloating a) => FilePath -> IO (Matrix2 a, Matrix2 a, Matrix2 a, Matrix2 a)
testLoadRGBA filename = do
    file <- readImageFromBMP filename
    case file of
        Right mat -> return $ M.unzip4 $ M.map (convert . unpackRGBA32) (Raw mat)
        Left e -> error $ "Unable to load file: " P.++ show e
    where convert t = let (r, g, b, a) = A.unlift t :: (Exp A.Word8, Exp A.Word8, Exp A.Word8, Exp A.Word8)
                      in A.lift (A.fromIntegral r / 255, A.fromIntegral g / 255, A.fromIntegral b / 255, A.fromIntegral a / 255)

testSaveRGBA :: (Elt a, IsFloating a) => FilePath -> Matrix2 a -> Matrix2 a -> Matrix2 a -> Matrix2 a -> IO ()
testSaveRGBA filename r g b a = writeImageToBMP filename $ compute' run $ M.map packRGBA32 $ zip4 (conv r) (conv g) (conv b) (conv a)
    where conv = M.map (A.truncate . (* 255.0) . clamp' 0 1)


testSaveChan :: (Elt a, IsFloating a) => FilePath -> Matrix2 a -> IO ()
testSaveChan filename pre = writeImageToBMP filename $ compute' run $ M.map rgba32OfFloat output
    where output = M.map conv pre
          conv (clamp' 0 1 -> x) = A.lift (x, x, x, x)
