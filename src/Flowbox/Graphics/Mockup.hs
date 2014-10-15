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

import qualified Codec.Picture.Png          as Juicy
import qualified Codec.Picture.Types        as Juicy
import qualified Data.Array.Accelerate      as A
import qualified Data.Array.Accelerate.IO   as A
import           Data.Array.Accelerate.CUDA
import qualified Data.Vector.Storable       as SV
import           Data.Char                  (toLower)
import           Math.Coordinate.Cartesian
import           Math.Space.Space
import           Linear                     (V2(..))

import qualified Flowbox.Graphics.Color                               as Color
import           Flowbox.Graphics.Composition.Generators.Filter
import           Flowbox.Graphics.Composition.Generators.Filter       as Conv
import           Flowbox.Graphics.Composition.Generators.Gradient
import           Flowbox.Graphics.Composition.Generators.Keyer
import           Flowbox.Graphics.Composition.Generators.Matrix
import           Flowbox.Graphics.Composition.Generators.Noise.Billow
import           Flowbox.Graphics.Composition.Generators.Noise.Perlin
import           Flowbox.Graphics.Composition.Generators.Pipe
import           Flowbox.Graphics.Composition.Generators.Rasterizer
import           Flowbox.Graphics.Composition.Generators.Sampler
import           Flowbox.Graphics.Composition.Generators.Shape
import           Flowbox.Graphics.Composition.Generators.Stencil
import           Flowbox.Graphics.Composition.Generators.Structures
import           Flowbox.Graphics.Composition.Generators.Transform
import           Flowbox.Graphics.Image.Channel
import           Flowbox.Graphics.Image.Color
import           Flowbox.Graphics.Image.Image                         as Image
import           Flowbox.Graphics.Image.IO.ImageMagick                (loadImage, saveImage)
import           Flowbox.Graphics.Image.View                          as View
import           Flowbox.Graphics.Utils
import           Flowbox.Math.Matrix                                  as M
import           Flowbox.Prelude                                      as P hiding (lookup)

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
testSaveRGBA filename r g b a = saveImageJuicy filename $ compute' run $ M.map A.packRGBA32 $ M.zip4 (conv r) (conv g) (conv b) (conv a)
    where conv = M.map (A.truncate . (* 255.0) . clamp' 0 1)

saveImageJuicy file matrix = do
    let ((), vec) = A.toVectors matrix
        A.Z A.:. h A.:. w = A.arrayShape matrix
    Juicy.writePng file $ (Juicy.Image w h (SV.unsafeCast vec) :: Juicy.Image Juicy.PixelRGBA8)

pattern VPS x = Value (Pure (Safe x))
type VPS x = Value Pure Safe x

defocus :: VPS Int -> Matrix2 Double -> Matrix2 Double
defocus (VPS size) = process
    where kernel = ellipse (pure $ variable size) 1 (0 :: A.Exp Double)
          process = rasterizer . normStencil (+) kernel (+) 0 . fromMatrix A.Clamp

motionBlur :: VPS Int -> VPS Double -> Matrix2 Double -> Matrix2 Double
motionBlur (VPS size) (VPS angle) = process
    where kernel = monosampler
                 $ rotateCenter (variable angle)
                 $ nearest
                 $ rectangle (Grid (variable size) 1) 1 0
          process = rasterizer . normStencil (+) kernel (+) 0 . fromMatrix A.Clamp

-- rotateCenter :: (Elt a, IsFloating a) => Exp a -> CartesianGenerator (Exp a) b -> CartesianGenerator (Exp a) b
rotateCenter phi = canvasT (fmap A.ceiling . rotate phi . asFloating) . onCenter (rotate phi)

bilateral :: VPS Double
          -> VPS Double
          -> VPS Int
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

offsetLuna :: VPS Double -> A.Exp Double -> A.Exp Double
offsetLuna (VPS (variable -> v)) = offset v

contrastLuna :: VPS Double -> A.Exp Double -> A.Exp Double
contrastLuna (VPS (variable -> v)) = contrast v

exposureLuna :: VPS Double -> VPS Double -> A.Exp Double -> A.Exp Double
exposureLuna (VPS (variable -> blackpoint)) (VPS (variable -> ex)) = exposure blackpoint ex

colorCorrectLuna :: VPS Double -> VPS Double -> VPS Double -> VPS Double -> VPS Double -> A.Exp (Color.RGB Double) -> A.Exp (Color.RGB Double)
colorCorrectLuna (VPS (variable -> saturation'))
                 (VPS (variable -> contrast'))
                 (VPS (variable -> gamma'))
                 (VPS (variable -> gain'))
                 (VPS (variable -> offset')) =
                    A.lift1 $ colorCorrect saturation' contrast' gamma' gain' offset'

gradeLuna :: VPS Double -> VPS Double -> VPS Double -> VPS Double -> VPS Double -> VPS Double -> VPS Double -> A.Exp Double -> A.Exp Double
gradeLuna (VPS (variable -> blackpoint))
          (VPS (variable -> whitepoint))
          (VPS (variable -> lift))
          (VPS (variable -> gain))
          (VPS (variable -> multiply'))
          (VPS (variable -> offset'))
          (VPS (variable -> gamma')) =
            grade blackpoint whitepoint lift gain multiply' offset' gamma'

saturateLuna :: VPS Double -> A.Exp (Color.RGB Double) -> A.Exp (Color.RGB Double)
saturateLuna (VPS (variable -> s)) = A.lift1 $ (saturate s :: Color.RGB (A.Exp Double) -> Color.RGB (A.Exp Double))

posterizeLuna :: VPS Double -> A.Exp Double -> A.Exp Double
posterizeLuna (VPS (variable -> colors)) = posterize colors

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

onEachRGB :: (A.Exp (Color.RGB Double) -> A.Exp (Color.RGB Double)) -> Image RGBA -> Image RGBA
onEachRGB f img = img'
    where rgb = unsafeGetRGB img
          Just view = lookup "rgba" img
          rgb' = M.map f rgb

          unzipRGB = M.unzip3 . M.map (\(A.unlift -> Color.RGB x y z) -> A.lift (x, y, z))

          (r', g', b') = unzipRGB rgb'

          v1 = View.append (ChannelFloat "r" (FlatData r')) view
          v2 = View.append (ChannelFloat "g" (FlatData g')) v1
          v3 = View.append (ChannelFloat "b" (FlatData b')) v2

          Right img' = Image.update (const $ Just v3) "rgba" img

keyer' :: (A.Exp (Color.RGB Double) -> A.Exp Double) -> Image RGBA -> Image RGBA
keyer' f img = img'
    where rgb = unsafeGetRGB img
          Just view = lookup "rgba" img
          alpha = M.map f rgb

          view' = View.append (ChannelFloat "a" (FlatData alpha)) view

          Right img' = Image.update (const $ Just view') "rgba" img

unsafeGetRGB :: Image RGBA -> M.Matrix2 (Color.RGB Double)
unsafeGetRGB img = rgb
    where Just view = lookup "rgba" img
          Right (Just (ChannelFloat _ (FlatData r))) = View.get view "r"
          Right (Just (ChannelFloat _ (FlatData g))) = View.get view "g"
          Right (Just (ChannelFloat _ (FlatData b))) = View.get view "b"

          rgb = M.zipWith3 (\x y z -> A.lift $ Color.RGB x y z) r g b

keyerLuna :: VPS KeyerMode -> VPS Double -> VPS Double -> VPS Double -> VPS Double -> Image RGBA -> Image RGBA
keyerLuna (VPS mode) (VPS (variable -> a)) (VPS (variable -> b)) (VPS (variable -> c)) (VPS (variable -> d)) img =
    keyer' (keyer mode (A.lift $ (a, b, c, d))) img

differenceKeyer' :: (A.Exp (Color.RGB Double) -> A.Exp (Color.RGB Double) -> A.Exp Double) -> Image RGBA -> Image RGBA -> Image RGBA
differenceKeyer' f background foreground = img'
    where backgroundRGB = unsafeGetRGB background
          foregroundRGB = unsafeGetRGB foreground

          alpha = M.map (A.uncurry f) $ M.zip backgroundRGB foregroundRGB

          Just view = lookup "rgba" foreground
          view' = View.append (ChannelFloat "a" (FlatData alpha)) view

          Right img' = Image.update (const $ Just view') "rgba" foreground

differenceKeyerLuna :: VPS Double -> VPS Double -> Image RGBA -> Image RGBA -> Image RGBA
differenceKeyerLuna (VPS (variable -> offset)) (VPS (variable -> gain)) background foreground = img'
    where diff = differenceKeyer offset gain
          img' = differenceKeyer' diff background foreground

cornerPinLuna :: VPS Double -> VPS Double
              -> VPS Double -> VPS Double
              -> VPS Double -> VPS Double
              -> VPS Double -> VPS Double
              -> Image RGBA
              -> Image RGBA
cornerPinLuna (VPS (variable -> p1x)) (VPS (variable -> p1y))
              (VPS (variable -> p2x)) (VPS (variable -> p2y))
              (VPS (variable -> p3x)) (VPS (variable -> p3y))
              (VPS (variable -> p4x)) (VPS (variable -> p4y)) img = img'
    where img' = onEachChannel process img
          process = rasterizer . monosampler . cornerPin (p1, p2, p3, p4) . nearest . fromMatrix (A.Constant 0)
          p1 = Point2 p1x p1y
          p2 = Point2 p2x p2y
          p3 = Point2 p3x p3y
          p4 = Point2 p4x p4y

gaussianLuna :: VPS Int -> Image RGBA -> Image RGBA
gaussianLuna (VPS (variable -> kernelSize)) img = img'
    where img' = onEachChannel process img
          hmat = id M.>-> normalize $ toMatrix (Grid 1 kernelSize) $ gauss 1.0
          vmat = id M.>-> normalize $ toMatrix (Grid kernelSize 1) $ gauss 1.0
          p = pipe A.Clamp
          process x = rasterizer $ id `p` Conv.filter 1 vmat `p` Conv.filter 1 hmat `p` id $ fromMatrix A.Clamp x

laplacianLuna :: VPS Int -> VPS Double -> VPS Double -> Image RGBA -> Image RGBA
laplacianLuna (VPS (variable -> kernSize)) (VPS (variable -> crossVal)) (VPS (variable -> sideVal)) img = img'
    where img' = onEachChannel process img
          process x = rasterizer $ id `p` Conv.filter 1 flt `p` id $ fromMatrix A.Clamp x
          flt = laplacian crossVal sideVal $ pure kernSize
          p = pipe A.Clamp

circularLuna :: Int -> Int -> Image RGBA
circularLuna = gradientLuna circularShape

conicalLuna :: Int -> Int -> Image RGBA
conicalLuna = gradientLuna conicalShape

squareLuna :: Int -> Image RGBA
squareLuna side = gradientLuna squareShape side side

gradientLuna gradient (variable -> width) (variable -> height) = channelToImageRGBA grad
    where grad = rasterizer $ monosampler $ gradientGenerator

          gradientGenerator = scale (Grid width height) $ translate (V2 0.5 0.5) $ mapper gray gradient
          gray   = [Tick 0.0 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Double Double Double]

          weightFun tickPos val1 weight1 val2 weight2 = mix tickPos val1 val2
          mapper = flip colorMapper weightFun

channelToImageRGBA :: Matrix2 Double -> Image RGBA
channelToImageRGBA m = image
    where image = singleton view
          view = View.empty "rgba"
               & View.append (ChannelFloat "r" . FlatData $ m)
               & View.append (ChannelFloat "g" . FlatData $ m)
               & View.append (ChannelFloat "b" . FlatData $ m)
               & View.append (ChannelFloat "a" . FlatData $ alpha)

          alpha :: Matrix2 Double
          alpha = M.generate (M.shape m) (const 1)

perlinLuna :: Double -> Int -> Int -> Image RGBA
perlinLuna (variable -> z) = noiseLuna (perlinNoise z)

billowLuna :: Double -> Int -> Int -> Image RGBA
billowLuna (variable -> z) = noiseLuna (billowNoise z)

noiseLuna noise (variable -> width) (variable -> height) = channelToImageRGBA noise'
    where noise' = rasterizer $ monosampler $ noiseGenerator

          noiseGenerator = scale (Grid width height) noise

rotateCenterLuna :: VPS Double -> Matrix2 Double -> Matrix2 Double
rotateCenterLuna (VPS (variable -> angle)) = rasterizer . monosampler . rotateCenter angle . nearest . fromMatrix (A.Constant 0)

test = channelToImageRGBA $ M.generate (A.index2 5 5) (const 0.8)
