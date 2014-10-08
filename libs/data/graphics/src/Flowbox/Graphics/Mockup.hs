---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Flowbox.Graphics.Mockup where

import qualified Data.Array.Accelerate      as A
import qualified Data.Array.Accelerate.IO   as A
import           Data.Array.Accelerate.CUDA
import           Data.Char                  (toLower)
import           Math.Coordinate.Cartesian
import           Math.Space.Space

import           Flowbox.Graphics.Composition.Generators.Filter
import           Flowbox.Graphics.Composition.Generators.Matrix
import           Flowbox.Graphics.Composition.Generators.Pipe
import           Flowbox.Graphics.Composition.Generators.Rasterizer
import           Flowbox.Graphics.Composition.Generators.Sampler
import           Flowbox.Graphics.Composition.Generators.Shape
import           Flowbox.Graphics.Composition.Generators.Stencil
import           Flowbox.Graphics.Composition.Generators.Structures
import           Flowbox.Graphics.Composition.Generators.Transform
import           Flowbox.Graphics.Image.Channel
import           Flowbox.Graphics.Image.Image          as Image
import           Flowbox.Graphics.Image.IO.ImageMagick (loadImage, saveImage)
import           Flowbox.Graphics.Image.View           as View
import           Flowbox.Graphics.Utils
import           Flowbox.Math.Matrix                   as M
import           Flowbox.Prelude                       as P hiding (lookup)

import Luna.Target.HS (Value(..), Safe(..), Pure(..), val, autoLift1, autoLift, fromValue)



testLoadRGBA' :: Value Pure Safe String -> Value IO Safe (Value Pure Safe (Matrix2 Double), Value Pure Safe (Matrix2 Double), Value Pure Safe (Matrix2 Double), Value Pure Safe (Matrix2 Double))
testLoadRGBA' path = autoLift1 ((fmap.fmap) (over each val) $ testLoadRGBA) path

testLoadRGBA :: FilePath -> IO (Matrix2 Double, Matrix2 Double, Matrix2 Double, Matrix2 Double)
testLoadRGBA filename = do
    file <- loadImage filename
    case file of
        Right mat -> return $ M.unzip4 $ M.map (convert . A.unpackRGBA32) (Raw mat)
        Left e -> error $ "Unable to load file: " P.++ show e
    where convert t = let (r, g, b, a) = A.unlift t :: (A.Exp A.Word8, A.Exp A.Word8, A.Exp A.Word8, A.Exp A.Word8)
                      in A.lift (A.fromIntegral r / 255, A.fromIntegral g / 255, A.fromIntegral b / 255, A.fromIntegral a / 255)

testSaveRGBA :: FilePath -> Matrix2 Double -> Matrix2 Double -> Matrix2 Double -> Matrix2 Double -> IO ()
testSaveRGBA filename r g b a = saveImage filename $ compute' run $ M.map A.packRGBA32 $ M.zip4 (conv r) (conv g) (conv b) (conv a)
    where conv = M.map (A.truncate . (* 255.0) . clamp' 0 1)

pattern VPS x = Value (Pure (Safe x))

defocus :: Value Pure Safe Int -> Matrix2 Double -> Matrix2 Double
defocus (VPS size) = process
    where kernel = ellipse (pure $ variable size) 1 (0 :: A.Exp Double)
          process = rasterizer . normStencil (+) kernel (+) 0 . fromMatrix A.Clamp

motionBlur :: Value Pure Safe Int -> Value Pure Safe Double -> Matrix2 Double -> Matrix2 Double
motionBlur (VPS size) (VPS angle) = process
    where kernel = monosampler
                 $ rotateCenter (variable angle)
                 $ nearest
                 $ rectangle (Grid (variable size) 1) 1 0
          process = rasterizer . normStencil (+) kernel (+) 0 . fromMatrix A.Clamp

-- rotateCenter :: (Elt a, IsFloating a) => Exp a -> CartesianGenerator (Exp a) b -> CartesianGenerator (Exp a) b
rotateCenter phi = canvasT (fmap A.ceiling . rotate phi . asFloating) . onCenter (rotate phi)

bilateral :: Value Pure Safe Double
          -> Value Pure Safe Double
          -> Value Pure Safe Int
          -> Matrix2 Double
          -> Matrix2 Double
bilateral (VPS psigma) (VPS csigma) (VPS (variable -> size)) = process
    where p = pipe A.Clamp
          spatial :: Generator (Point2 (Exp Int)) (Exp Double)
          spatial = Generator (pure $ variable size) $ \(Point2 x y) ->
              let dst = sqrt . A.fromIntegral $ (x - size `div` 2) * (x - size `div` 2) + (y - size `div` 2) * (y - size `div` 2)
              in apply (gauss $ variable psigma) dst
          domain center neighbour = apply (gauss $ variable csigma) (abs $ neighbour - center)
          process = rasterizer . (id `p` bilateralStencil (+) spatial domain (+) 0 `p` id) . fromMatrix A.Clamp

data Chan = R | G | B | A
          deriving Show

-- loadChannel :: FilePath -> Chan -> IO Channel
-- loadChannel path chan = fmap (ChannelFloat (P.map toLower $ show chan) . FlatData . get chan) (testLoadRGBA path)
--     where get :: Chan -> Matrix2 (Double, Double, Double, Double) -> Matrix2 Double
--           get R = M.map (\(A.unlift -> (r, _, _, _)::(A.Exp Double, A.Exp Double, A.Exp Double, A.Exp Double)) -> r)
--           get G = M.map (\(A.unlift -> (_, g, _, _)::(A.Exp Double, A.Exp Double, A.Exp Double, A.Exp Double)) -> g)
--           get B = M.map (\(A.unlift -> (_, _, b, _)::(A.Exp Double, A.Exp Double, A.Exp Double, A.Exp Double)) -> b)
--           get A = M.map (\(A.unlift -> (_, _, _, a)::(A.Exp Double, A.Exp Double, A.Exp Double, A.Exp Double)) -> a)


-- ====== On 4-tuples ======
-- testLoadRGBA :: FilePath -> IO (Matrix2 (Double, Double, Double, Double))
-- testLoadRGBA filename = do
--     file <- loadImage filename
--     case file of
--         Right mat -> return $ M.map (convert . A.unpackRGBA32) (Raw mat)
--         Left e -> error $ "Unable to load file: " P.++ show e
--     where convert t = let (r, g, b, a) = A.unlift t :: (A.Exp A.Word8, A.Exp A.Word8, A.Exp A.Word8, A.Exp A.Word8)
--                       in A.lift (A.fromIntegral r / 255, A.fromIntegral g / 255, A.fromIntegral b / 255, A.fromIntegral a / 255)

-- testSaveRGBA :: FilePath -> Matrix2 (Double, Double, Double, Double) -> IO ()
-- testSaveRGBA filename rgba = saveImage filename $ compute' run $ M.map A.packRGBA32 $ M.map (\(A.unlift -> (r, g, b, a)) -> A.lift (conv r, conv g, conv b, conv a)) rgba
--     where conv = A.truncate . (* 255.0) . clamp' 0 1

-- defocus :: Int -> Matrix2 (Double, Double, Double, Double) -> Matrix2 (Double, Double, Double, Double)
-- defocus size = (\(a, b, c, d) -> M.zip4 a b c d) . over each process . M.unzip4
--     where kernel = ellipse (pure $ variable size) 1 (0 :: A.Exp Double)
--           process = rasterizer . normStencil (+) kernel (+) 0 . fromMatrix A.Clamp

-- motionBlur :: Int -> Double -> Matrix2 (Double, Double, Double, Double) -> Matrix2 (Double, Double, Double, Double)
-- motionBlur size angle = (\(a, b, c, d) -> M.zip4 a b c d) . over each process . M.unzip4
--     where kernel = monosampler
--                  $ rotateCenter (variable angle)
--                  $ nearest
--                  $ rectangle (Grid (variable size) 1) 1 0
--           process = rasterizer . normStencil (+) kernel (+) 0 . fromMatrix A.Clamp

-- -- rotateCenter :: (Elt a, IsFloating a) => Exp a -> CartesianGenerator (Exp a) b -> CartesianGenerator (Exp a) b
-- rotateCenter phi = canvasT (fmap A.ceiling . rotate phi . asFloating) . onCenter (rotate phi)

-- bilateral :: Double -> Double -> Int -> Matrix2 (Double, Double, Double, Double) -> Matrix2 (Double, Double, Double, Double)
-- bilateral (variable -> psigma) (variable -> csigma) (variable -> size) = (\(a, b, c, d) -> M.zip4 a b c d) . over each process . M.unzip4
--     where p = pipe A.Clamp
--           spatial :: Generator (Point2 (Exp Int)) (Exp Double)
--           spatial = Generator (pure $ size) $ \(Point2 x y) ->
--               let dst = sqrt . A.fromIntegral $ (x - size `div` 2) * (x - size `div` 2) + (y - size `div` 2) * (y - size `div` 2)
--               in apply (gauss psigma) dst
--           domain center neighbour = apply (gauss csigma) (abs $ neighbour - center)
--           process = rasterizer . (id `p` bilateralStencil (+) spatial domain (+) 0 `p` id) . fromMatrix A.Clamp

-- ====== END ======


loadImageLuna :: FilePath -> IO (Image RGBA)
loadImageLuna path = do
    (r, g, b, a) <- testLoadRGBA path
    let view = View.empty "rgba"
             & View.append (ChannelFloat "r" . FlatData $ r)
             & View.append (ChannelFloat "g" . FlatData $ g)
             & View.append (ChannelFloat "b" . FlatData $ b)
             & View.append (ChannelFloat "a" . FlatData $ a)
        image = singleton view
    return image

saveImageLuna :: FilePath -> Image RGBA -> IO (Image RGBA)
saveImageLuna path img = do
    let Just view = lookup "rgba" img
        Right (Just (ChannelFloat _ (FlatData r))) = View.get view "r"
        Right (Just (ChannelFloat _ (FlatData g))) = View.get view "g"
        Right (Just (ChannelFloat _ (FlatData b))) = View.get view "b"
        Right (Just (ChannelFloat _ (FlatData a))) = View.get view "a"
    testSaveRGBA path r g b a
    return img

onEachChannel :: (Matrix2 Double -> Matrix2 Double) -> Image RGBA -> Image RGBA
onEachChannel f img = res
    where Right res = Image.map (View.map fChan) img

          fChan :: Channel -> Channel
          fChan (ChannelFloat name flatdata) = ChannelFloat name (flatdata & matrix %~ f)

onEachValue :: (A.Exp Double -> A.Exp Double) -> Image RGBA -> Image RGBA
onEachValue f img = res
    where Right res = Image.map (View.map f') img

          f' :: Channel -> Channel
          f' (ChannelFloat name flatdata) = ChannelFloat name (flatdata & matrix %~ (M.map f))
