---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Utils where

import           Flowbox.Prelude                      as P
import           Flowbox.Math.Matrix                  as M
import           Flowbox.Graphics.Utils

import qualified Data.Array.Accelerate                as A
import           Data.Array.Accelerate.IO
import           Data.Array.Accelerate.CUDA

testLoadRGBA :: FilePath -> IO (Matrix2 Double, Matrix2 Double, Matrix2 Double, Matrix2 Double)
testLoadRGBA filename = do
    file <- readImageFromBMP filename
    case file of
        Right mat -> return $ M.unzip4 $ M.map (convert . unpackRGBA32) (Raw mat)
        Left e -> error $ "Unable to load file: " P.++ show e
    where convert t = let (r, g, b, a) = A.unlift t :: (Exp A.Word8, Exp A.Word8, Exp A.Word8, Exp A.Word8)
                      in A.lift (A.fromIntegral r / 255, A.fromIntegral g / 255, A.fromIntegral b / 255, A.fromIntegral a / 255)

testSaveRGBA :: FilePath -> Matrix2 Double -> Matrix2 Double -> Matrix2 Double -> Matrix2 Double -> IO ()
testSaveRGBA filename r g b a = writeImageToBMP filename $ compute' run $ M.map packRGBA32 $ zip4 rChan gChan bChan aChan
    where rChan = M.map (A.truncate . (* 255.0) . clamp' 0 1) r
          gChan = M.map (A.truncate . (* 255.0) . clamp' 0 1) g
          bChan = M.map (A.truncate . (* 255.0) . clamp' 0 1) b
          aChan = M.map (A.truncate . (* 255.0) . clamp' 0 1) a

testSaveChan :: FilePath -> Matrix2 Double -> IO ()
testSaveChan filename a = testSaveRGBA filename a a a a
