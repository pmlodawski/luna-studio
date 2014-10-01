---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module Flowbox.Graphics.Mockup where

import qualified Data.Array.Accelerate      as A
import qualified Data.Array.Accelerate.IO   as A
import           Data.Array.Accelerate.CUDA
import           Math.Coordinate.Cartesian
import           Math.Space.Space

import Flowbox.Graphics.Composition.Generators.Filter
import Flowbox.Graphics.Composition.Generators.Matrix
import Flowbox.Graphics.Composition.Generators.Pipe
import Flowbox.Graphics.Composition.Generators.Rasterizer
import Flowbox.Graphics.Composition.Generators.Sampler
import Flowbox.Graphics.Composition.Generators.Shape
import Flowbox.Graphics.Composition.Generators.Stencil
import Flowbox.Graphics.Composition.Generators.Structures
import Flowbox.Graphics.Composition.Generators.Transform
import Flowbox.Graphics.Image.IO.ImageMagick (loadImage, saveImage)
import Flowbox.Graphics.Utils
import Flowbox.Math.Matrix                   as M
import Flowbox.Prelude                       as P

import Luna.Target.HS (Value(..), Safe(..), Pure(..), val, autoLift1, autoLift, fromValue)



testLoadRGBA' :: FilePath -> Value IO Safe (Matrix2 (Double, Double, Double, Double))
testLoadRGBA' path = autoLift $ testLoadRGBA path

testLoadRGBA :: FilePath -> IO (Matrix2 (Double, Double, Double, Double))
testLoadRGBA filename = do
    file <- loadImage filename
    case file of
        Right mat -> return $ M.map (convert . A.unpackRGBA32) (Raw mat)
        Left e -> error $ "Unable to load file: " P.++ show e
    where convert t = let (r, g, b, a) = A.unlift t :: (A.Exp A.Word8, A.Exp A.Word8, A.Exp A.Word8, A.Exp A.Word8)
                      in A.lift (A.fromIntegral r / 255, A.fromIntegral g / 255, A.fromIntegral b / 255, A.fromIntegral a / 255)

testSaveRGBA :: FilePath -> Matrix2 (Double, Double, Double, Double) -> IO ()
testSaveRGBA filename rgba = saveImage filename $ compute' run $ M.map A.packRGBA32 $ M.map (\(A.unlift -> (r, g, b, a)) -> A.lift (conv r, conv g, conv b, conv a)) rgba
    where conv = A.truncate . (* 255.0) . clamp' 0 1

defocus :: Int -> Matrix2 (Double, Double, Double, Double) -> Matrix2 (Double, Double, Double, Double)
defocus size = (\(a, b, c, d) -> M.zip4 a b c d) . over each process . M.unzip4
    where kernel = ellipse (pure $ variable size) 1 (0 :: A.Exp Double)
          process = rasterizer . normStencil (+) kernel (+) 0 . fromMatrix A.Clamp

motionBlur :: Int -> Double -> Matrix2 (Double, Double, Double, Double) -> Matrix2 (Double, Double, Double, Double)
motionBlur size angle = (\(a, b, c, d) -> M.zip4 a b c d) . over each process . M.unzip4
    where kernel = monosampler
                 $ rotateCenter (variable angle)
                 $ nearest
                 $ rectangle (Grid (variable size) 1) 1 0
          process = rasterizer . normStencil (+) kernel (+) 0 . fromMatrix A.Clamp

-- rotateCenter :: (Elt a, IsFloating a) => Exp a -> CartesianGenerator (Exp a) b -> CartesianGenerator (Exp a) b
rotateCenter phi = canvasT (fmap A.ceiling . rotate phi . asFloating) . onCenter (rotate phi)

bilateral :: Double -> Double -> Int -> Matrix2 (Double, Double, Double, Double) -> Matrix2 (Double, Double, Double, Double)
bilateral (variable -> psigma) (variable -> csigma) (variable -> size) = (\(a, b, c, d) -> M.zip4 a b c d) . over each process . M.unzip4
    where p = pipe A.Clamp
          spatial :: Generator (Point2 (Exp Int)) (Exp Double)
          spatial = Generator (pure $ size) $ \(Point2 x y) ->
              let dst = sqrt . A.fromIntegral $ (x - size `div` 2) * (x - size `div` 2) + (y - size `div` 2) * (y - size `div` 2)
              in apply (gauss psigma) dst
          domain center neighbour = apply (gauss csigma) (abs $ neighbour - center)
          process = rasterizer . (id `p` bilateralStencil (+) spatial domain (+) 0 `p` id) . fromMatrix A.Clamp
