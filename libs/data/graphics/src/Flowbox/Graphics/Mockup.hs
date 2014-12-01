---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}

module Flowbox.Graphics.Mockup (
      module Flowbox.Graphics.Mockup
    , module Math.Metric
    , module Math.Space.Space
    , A.Boundary(..)
    , readFromEXR
    , variable
) where

import qualified Codec.Picture.Png                 as Juicy
import qualified Codec.Picture.Types               as Juicy
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Array.Sugar as A
import           Data.Array.Accelerate.CUDA
import qualified Data.Array.Accelerate.IO          as A
import           Data.Char                         (toLower)
import qualified Data.Vector.Storable              as SV
import           Math.Coordinate.Cartesian
import           Math.Space.Space
import           Math.Metric
import           Linear                            (V2(..))

import qualified Flowbox.Graphics.Color                               as Color
import qualified Flowbox.Graphics.Color.Companding                    as Gamma
import           Flowbox.Graphics.Composition.Dither
import           Flowbox.Geom2D.ControlPoint
import           Flowbox.Geom2D.Path
import qualified Flowbox.Geom2D.Shape                                 as GShape
import qualified Flowbox.Geom2D.Mask as Mask
import           Flowbox.Geom2D.Rasterizer
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
import           Flowbox.Graphics.Composition.Generators.Stencil      as Stencil
import           Flowbox.Graphics.Composition.Generators.Structures
import           Flowbox.Graphics.Composition.Generators.Transform
import           Flowbox.Graphics.Composition.Histogram
import qualified Flowbox.Graphics.Composition.Raster                  as Raster
import           Flowbox.Graphics.Image.Channel
import           Flowbox.Graphics.Image.Color
import           Flowbox.Graphics.Image.Image                         as Image
import           Flowbox.Graphics.Image.Error                         as Image
import           Flowbox.Graphics.Image.IO.ImageMagick                (loadImage, saveImage)
import           Flowbox.Graphics.Image.IO.OpenEXR                    (readFromEXR)
import           Flowbox.Graphics.Image.Merge                         (AlphaBlend(..))
import qualified Flowbox.Graphics.Image.Merge                         as Merge
import           Flowbox.Graphics.Image.View                          as View
import           Flowbox.Graphics.Utils
import           Flowbox.Math.Matrix                                  as M
import           Flowbox.Prelude                                      as P hiding (lookup)

import Luna.Target.HS (Pure (..), Safe (..), Value (..), autoLift, autoLift1, fromValue, val)
import Control.PolyApplicative ((<<*>>))



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

saveImageJuicy :: forall e a.
                        (SV.Storable a, Elt e,
                         A.Vectors (A.EltRepr e)
                         ~ ((), SV.Vector a)) =>
                        FilePath -> A.Array ((Z :. Int) :. Int) e -> IO ()
saveImageJuicy file matrix = do
    let ((), vec) = A.toVectors matrix
        A.Z A.:. h A.:. w = A.arrayShape matrix
    Juicy.writePng file $ (Juicy.Image w h (SV.unsafeCast vec) :: Juicy.Image Juicy.PixelRGBA8)

pattern VPS x = Value (Pure (Safe x))
type VPS x = Value Pure Safe x

defocus :: Int -> Image -> Image
defocus size = onEachChannel process
    where kernel = ellipse (pure $ variable size) 1 (0 :: A.Exp Double)
          process = rasterizer . normStencil (+) kernel (+) 0 . fromMatrix A.Clamp

motionBlur :: Int -> Double -> Image -> Image
motionBlur size angle = onEachChannel process
    where kernel = monosampler
                 $ rotateCenter (variable angle)
                 $ nearest
                 $ rectangle (Grid (variable size) 1) 1 0
          process = rasterizer . normStencil (+) kernel (+) 0 . fromMatrix A.Clamp

-- rotateCenter :: (Elt a, IsFloating a) => Exp a -> CartesianGenerator (Exp a) b -> CartesianGenerator (Exp a) b
rotateCenter phi = canvasT (fmap A.ceiling . rotate phi . asFloating) . onCenter (rotate phi)

bilateral :: Double
          -> Double
          -> Int
          -> Image
          -> Image
bilateral psigma csigma (variable -> size) = onEachChannel process
    where p = pipe A.Clamp
          spatial :: Generator (Point2 (Exp Int)) (Exp Double)
          spatial = Generator (pure $ variable size) $ \(Point2 x y) ->
              let dst = sqrt . A.fromIntegral $ (x - size `div` 2) * (x - size `div` 2) + (y - size `div` 2) * (y - size `div` 2)
              in apply (gauss $ variable psigma) dst
          domain center neighbour = apply (gauss $ variable csigma) (abs $ neighbour - center)
          process = rasterizer . (id `p` bilateralStencil (+) spatial domain (+) 0 `p` id) . fromMatrix A.Clamp

offsetLuna :: Color.RGBA Double -> Image -> Image
offsetLuna (fmap variable -> Color.RGBA r g b a) = onImageRGBA (offset r) (offset g) (offset b) (offset a)

contrastLuna :: Color.RGBA Double -> Image -> Image
contrastLuna (fmap variable -> Color.RGBA r g b a) = onImageRGBA (contrast r) (contrast g) (contrast b) (contrast a)

exposureLuna :: Color.RGBA Double -> Color.RGBA Double -> Image -> Image
exposureLuna (fmap variable -> Color.RGBA blackpointR blackpointG blackpointB blackpointA)
             (fmap variable -> Color.RGBA exR exG exB exA) =
                 onImageRGBA (exposure blackpointR exR)
                             (exposure blackpointG exG)
                             (exposure blackpointB exB)
                             (exposure blackpointA exA)

gradeLuna :: VPS Double -> VPS Double -> VPS Double -> Double -> Double -> Double -> Double -> Image -> Image
gradeLuna (VPS (variable -> blackpoint))
          (VPS (variable -> whitepoint))
          (VPS (variable -> lift))
          (variable -> gain)
          (variable -> multiply')
          (variable -> offset')
          (variable -> gamma') =
            onEachValue $ grade blackpoint whitepoint lift gain multiply' offset' gamma'

saturateLuna :: Double -> Image -> Image
saturateLuna (variable -> s) = onEachRGB $ A.lift1 $ (saturate s :: Color.RGB (A.Exp Double) -> Color.RGB (A.Exp Double))

posterizeLuna :: Double -> Image -> Image
posterizeLuna (variable -> colors) = onEachValue $ posterize colors

loadImageLuna :: FilePath -> IO Image
loadImageLuna path = do
    (r, g, b, a) <- testLoadRGBA path
    let view = insertChannelFloats (View.empty "rgba") [
                   ("rgba.r", r)
                 , ("rgba.g", g)
                 , ("rgba.b", b)
                 , ("rgba.a", a)
               ]
        image = singleton view
    return image

insertChannelFloats :: View -> [(String, Matrix2 Double)] -> View
insertChannelFloats view chans = foldr f view chans
    where f (name, chan) acc = View.append (ChannelFloat name . FlatData $ chan) acc

saveImageLuna :: FilePath -> Image -> IO Image
saveImageLuna path img = do
    let (r, g, b, a) = unsafeGetChannels img
    testSaveRGBA path r g b a
    return img

onEachChannel :: (Matrix2 Double -> Matrix2 Double) -> Image -> Image
onEachChannel f img = res
    where res = Image.map (View.map fChan) img
          fChan :: Channel -> Channel
          fChan (ChannelFloat name flatdata) = ChannelFloat name (flatdata & matrix %~ f)

onEachValue :: (A.Exp Double -> A.Exp Double) -> Image -> Image
onEachValue f img = res
    where res = Image.map (View.map f') img

          f' :: Channel -> Channel
          f' (ChannelFloat name flatdata) = ChannelFloat name (flatdata & matrix %~ (M.map f))

onEachRGB :: (A.Exp (Color.RGB Double) -> A.Exp (Color.RGB Double)) -> Image -> Image
onEachRGB f img = img'
    where rgb = unsafeGetRGB img
          Just view = lookup "rgba" img
          rgb' = M.map f rgb

          unzipRGB = M.unzip3 . M.map (\(A.unlift -> Color.RGB x y z) -> A.lift (x, y, z))

          (r', g', b') = unzipRGB rgb'

          view' = insertChannelFloats view [
                      ("rgba.r", r')
                    , ("rgba.g", g')
                    , ("rgba.b", b')
                  ]

          img' = Image.update (const $ Just view') "rgba" img

keyer' :: (A.Exp (Color.RGB Double) -> A.Exp Double) -> Image -> Image
keyer' f img = img'
    where rgb = unsafeGetRGB img
          Just view = lookup "rgba" img
          alpha = M.map f rgb

          view' = insertChannelFloats view [("rgba.a", alpha)]

          img' = Image.update (const $ Just view') "rgba" img

unsafeGetRGB :: Image -> M.Matrix2 (Color.RGB Double)
unsafeGetRGB img = rgb
    where (r, g, b, _) = unsafeGetChannels img

          rgb = M.zipWith3 (\x y z -> A.lift $ Color.RGB x y z) r g b

unsafeGetChannels :: Image -> (M.Matrix2 Double, M.Matrix2 Double, M.Matrix2 Double, M.Matrix2 Double)
unsafeGetChannels img = (r, g, b, a)
    where Just view = lookup "rgba" img
          Right (Just (ChannelFloat _ (FlatData r))) = View.get view "rgba.r"
          Right (Just (ChannelFloat _ (FlatData g))) = View.get view "rgba.g"
          Right (Just (ChannelFloat _ (FlatData b))) = View.get view "rgba.b"
          Right (Just (ChannelFloat _ (FlatData a))) = View.get view "rgba.a"

keyerLuna :: KeyerMode -> Double -> Double -> Double -> Double -> Image -> Image
keyerLuna mode (variable -> a) (variable -> b) (variable -> c) (variable -> d) img =
    keyer' (keyer mode (A.lift $ (a, b, c, d))) img

differenceKeyer' :: (A.Exp (Color.RGB Double) -> A.Exp (Color.RGB Double) -> A.Exp Double) -> Image -> Image -> Image
differenceKeyer' f background foreground = img'
    where backgroundRGB = unsafeGetRGB background
          foregroundRGB = unsafeGetRGB foreground

          alpha = M.map (A.uncurry f) $ M.zip backgroundRGB foregroundRGB

          Just view = lookup "rgba" foreground
          view' = insertChannelFloats view [("rgba.a", alpha)]

          img' = Image.update (const $ Just view') "rgba" foreground

differenceKeyerLuna :: Double -> Double -> Image -> Image -> Image
differenceKeyerLuna (variable -> offset) (variable -> gain) background foreground = img'
    where diff = differenceKeyer offset gain
          img' = differenceKeyer' diff background foreground

cornerPinLuna :: Double -> Double
              -> Double -> Double
              -> Double -> Double
              -> Double -> Double
              -> Image
              -> Image
cornerPinLuna (variable -> p1x) (variable -> p1y)
              (variable -> p2x) (variable -> p2y)
              (variable -> p3x) (variable -> p3y)
              (variable -> p4x) (variable -> p4y) img = img'
    where img' = onEachChannel process img
          process = rasterizer . monosampler . cornerPin (p1, p2, p3, p4) . nearest . fromMatrix (A.Constant 0)
          p1 = Point2 p1x p1y
          p2 = Point2 p2x p2y
          p3 = Point2 p3x p3y
          p4 = Point2 p4x p4y

gaussianLuna :: Int -> Image -> Image
gaussianLuna (variable -> kernelSize) img = img'
    where img' = onEachChannel process img
          hmat = id M.>-> normalize $ toMatrix (Grid 1 kernelSize) $ gauss 1.0
          vmat = id M.>-> normalize $ toMatrix (Grid kernelSize 1) $ gauss 1.0
          p = pipe A.Clamp
          process x = rasterizer $ id `p` Conv.filter 1 vmat `p` Conv.filter 1 hmat `p` id $ fromMatrix A.Clamp x

laplacianLuna :: Int -> Double -> Double -> Image -> Image
laplacianLuna (variable -> kernSize) (variable -> crossVal) (variable -> sideVal) img = img'
    where img' = onEachChannel process img
          process x = rasterizer $ id `p` Conv.filter 1 flt `p` id $ fromMatrix A.Clamp x
          flt = laplacian crossVal sideVal $ pure kernSize
          p = pipe A.Clamp

constantLuna :: Int -> Int -> Color.RGBA Double -> Image
constantLuna (variable -> width) (variable -> height) (fmap variable -> Color.RGBA r g b a) =
    Raster.constant (A.index2 height width) chans
    where chans = [ ("rgba.r", r)
                  , ("rgba.g", g)
                  , ("rgba.b", b)
                  , ("rgba.a", a)
                  ]

circularLuna :: Int -> Int -> Image
circularLuna = gradientLuna circularShape

conicalLuna :: Int -> Int -> Image
conicalLuna = gradientLuna conicalShape

squareLuna :: Int -> Image
squareLuna side = gradientLuna squareShape side side

diamondLuna :: Int -> Int -> Image
diamondLuna = gradientLuna diamondShape

radialShapeLuna :: (Metric a (Point2 (Exp Double)) (Exp Double), MetricCoord a Cartesian)
                => a -> Int -> Int -> Image
radialShapeLuna metric w h = gradientLuna (radialShape metric) w h

linearShapeLuna :: Int -> Int -> Image
linearShapeLuna = gradientLuna linearShape

gradientLuna :: forall e.
                      (A.Lift Exp e,
                       A.Plain e ~ Int) =>
                      Generator (Point2 (Exp Double)) (Exp Double) -> e -> e -> Image
gradientLuna gradient (variable -> width) (variable -> height) = channelToImageRGBA grad
    where grad = rasterizer $ monosampler $ gradientGenerator

          gradientGenerator = scale (Grid width height) $ translate (V2 0.5 0.5) $ mapper gray gradient
          gray   = [Tick 0.0 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Double Double Double]

          weightFun tickPos val1 weight1 val2 weight2 = mix tickPos val1 val2
          mapper = flip colorMapper weightFun

channelToImageRGBA :: Matrix2 Double -> Image
channelToImageRGBA m = image
    where image = singleton view
          view = insertChannelFloats (View.empty "rgba") [
                     ("rgba.r", m)
                   , ("rgba.g", m)
                   , ("rgba.b", m)
                   , ("rgba.a", alpha)
                 ]

          alpha :: Matrix2 Double
          alpha = M.generate (M.shape m) (const 1)

perlinLuna :: Double -> Int -> Int -> Image
perlinLuna (variable -> z) = noiseLuna (perlinNoise z)

billowLuna :: Double -> Int -> Int -> Image
billowLuna (variable -> z) = noiseLuna (billowNoise z)

noiseLuna :: forall e a.
                   (IsFloating a, Elt a, A.Lift Exp e,
                    A.Plain e ~ Int) =>
                   CartesianGenerator (Exp a) (Exp Double) -> e -> e -> Image
noiseLuna noise (variable -> width) (variable -> height) = channelToImageRGBA noise'
    where noise' = rasterizer $ monosampler $ noiseGenerator

          noiseGenerator = scale (Grid width height) noise

rotateCenterLuna :: Double -> Image -> Image
rotateCenterLuna (variable -> angle) = onEachChannel $ rasterizer . monosampler . rotateCenter angle . nearest . fromMatrix (A.Constant 0)

translateLuna :: A.Boundary (A.Exp Double) -> Double -> Double -> Image -> Image
translateLuna boundary (variable -> x) (variable -> y) = onEachChannel $ rasterizer . monosampler . translate (V2 x y) . nearest . fromMatrix boundary

scaleToLuna :: A.Boundary (A.Exp Double) -> Int -> Int -> Image -> Image
scaleToLuna boundary (variable -> x) (variable -> y) = onEachChannel $ rasterizer . monosampler . scale (Grid x y) . nearest . fromMatrix boundary

scaleLuna :: A.Boundary (A.Exp Double) -> Double -> Double -> Image -> Image
scaleLuna boundary (variable -> x) (variable -> y) = onEachChannel $ rasterizer . monosampler . canvasT f . scale (V2 x y) . interpolator (Conv.catmulRom) . fromMatrix boundary
    where f = fmap A.truncate . scale (V2 x y) . asFloating

hsvToolLuna :: VPS Double -> VPS Double -> VPS Double -> VPS Double
            -> VPS Double -> VPS Double -> VPS Double -> VPS Double
            -> VPS Double -> VPS Double -> VPS Double -> VPS Double
            -> A.Exp (Color.RGB Double)
            -> A.Exp (Color.RGB Double)
hsvToolLuna (VPS (variable -> hueRangeStart)) (VPS (variable -> hueRangeEnd))
            (VPS (variable -> hueRotation)) (VPS (variable -> hueRolloff))
            (VPS (variable -> saturationRangeStart)) (VPS (variable -> saturationRangeEnd))
            (VPS (variable -> saturationAdjustment)) (VPS (variable -> saturationRolloff))
            (VPS (variable -> brightnessRangeStart)) (VPS (variable -> brightnessRangeEnd))
            (VPS (variable -> brightnessAdjustment)) (VPS (variable -> brightnessRolloff)) =
    A.lift1 (hsvTool (A.lift $ Range hueRangeStart hueRangeEnd) hueRotation hueRolloff
                     (A.lift $ Range saturationRangeStart saturationRangeEnd) saturationAdjustment saturationRolloff
                     (A.lift $ Range brightnessRangeStart brightnessRangeEnd) brightnessAdjustment brightnessRolloff :: Color.RGB (A.Exp Double) -> Color.RGB (A.Exp Double))

hsvToolLuna' :: Double -> Double -> Double -> Double
             -> Double -> Double -> Double -> Double
             -> Double -> Double -> Double -> Double
             -> Image
             -> Image
hsvToolLuna' (variable -> hueRangeStart) (variable -> hueRangeEnd)
             (variable -> hueRotation) (variable -> hueRolloff)
             (variable -> saturationRangeStart) (variable -> saturationRangeEnd)
             (variable -> saturationAdjustment) (variable -> saturationRolloff)
             (variable -> brightnessRangeStart) (variable -> brightnessRangeEnd)
             (variable -> brightnessAdjustment) (variable -> brightnessRolloff) =
    onEachRGB $ A.lift1 (hsvTool (A.lift $ Range hueRangeStart hueRangeEnd) hueRotation hueRolloff
                     (A.lift $ Range saturationRangeStart saturationRangeEnd) saturationAdjustment saturationRolloff
                     (A.lift $ Range brightnessRangeStart brightnessRangeEnd) brightnessAdjustment brightnessRolloff :: Color.RGB (A.Exp Double) -> Color.RGB (A.Exp Double))

-- test :: VPS Double -> VPS Double -> VPS Double -> VPS Double
--      -> VPS Double -> VPS Double -> VPS Double -> VPS Double
--      -> VPS Double -> VPS Double -> VPS Double -> VPS Double
--      -> VPS (Image) -> VPS (Image)
test = liftF13 hsvToolLuna'

data MergeMode = Atop
           | Average
           | ColorBurn
           | ColorDodge
           | ConjointOver
           | Copy
           | Difference
           | DisjointOver
           | DivideBySource
           | DivideByDestination
           | Exclusion
           | From
           | Geometric
           | HardLight
           | Hypot
           | In
           | Mask
           | Matte
           -- | Max
           -- | Min
           | Minus
           | Multiply
           | Out
           | Over
           | Overlay
           | Plus
           | Screen
           | SoftLight
           | SoftLightPegtop
           | SoftLightIllusions
           | SoftLightPhotoshop
           | Stencil
           | Under
           | XOR
           deriving (Show)

mergeLuna :: MergeMode -> Merge.AlphaBlend -> Image -> Image -> Image
mergeLuna mode alphaBlend img1 img2 = case mode of
    Atop                -> processMerge $ Merge.threeWayMerge             Merge.atop
    Average             -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.average
    ColorBurn           -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.colorBurn
    ColorDodge          -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.colorDodge
    ConjointOver        -> processMerge $ Merge.threeWayMerge             Merge.conjointOver
    Copy                -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.copy
    Difference          -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.difference
    DisjointOver        -> processMerge $ Merge.threeWayMerge             Merge.disjointOver
    DivideBySource      -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.divideBySrc
    DivideByDestination -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.divideByDst
    Exclusion           -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.exclusion
    From                -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.from
    Geometric           -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.geometric
    HardLight           -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.hardLight
    Hypot               -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.hypot
    In                  -> processMerge $ Merge.threeWayMerge             Merge.inBlend
    Mask                -> processMerge $ Merge.threeWayMerge             Merge.withMask
    Matte               -> processMerge $ Merge.threeWayMerge             Merge.matte
    -- Max                 -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.max
    -- Min                 -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.min
    Minus               -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.minus
    Multiply            -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.multiply
    Out                 -> processMerge $ Merge.threeWayMerge             Merge.out
    Over                -> processMerge $ Merge.threeWayMerge             Merge.over
    Overlay             -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.overlayFun
    Plus                -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.plus
    Screen              -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.screen
    SoftLight           -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.softLight
    SoftLightPegtop     -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.softLightPegtop
    SoftLightIllusions  -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.softLightIllusions
    SoftLightPhotoshop  -> processMerge $ Merge.threeWayMerge' alphaBlend Merge.softLightPhotoshop
    Stencil             -> processMerge $ Merge.threeWayMerge             Merge.stencil
    Under               -> processMerge $ Merge.threeWayMerge             Merge.under
    XOR                 -> processMerge $ Merge.threeWayMerge             Merge.xor
    where processMerge f = img'
              where (r, g, b, a) = f r1 g1 b1 r2 g2 b2 a1 a2
                    view' = insertChannelFloats view [
                                ("rgba.r", rasterizer $ r)
                              , ("rgba.g", rasterizer $ g)
                              , ("rgba.b", rasterizer $ b)
                              , ("rgba.a", rasterizer $ a)
                            ]
                    img' = Image.update (const $ Just view') "rgba" img1
          Just view = lookup "rgba" img1
          (r1, g1, b1, a1) = unsafeGetChannels img1 & over each (fromMatrix (A.Constant 0))
          (r2, g2, b2, a2) = unsafeGetChannels img2 & over each (fromMatrix (A.Constant 0))

onGenerator f img = img'
    where (r, g, b, a) = unsafeGetChannels img & over each (rasterizer . f . fromMatrix (A.Constant 0))
          Just view = lookup "rgba" img
          view' = insertChannelFloats view [
                      ("rgba.r", r)
                    , ("rgba.g", g)
                    , ("rgba.b", b)
                    , ("rgba.a", a)
                  ]
          img' = Image.update (const $ Just view') "rgba" img

erodeLuna :: Int -> Image -> Image
erodeLuna (variable -> size) = onGenerator $ erode $ pure size

dilateLuna :: Int -> Image -> Image
dilateLuna (variable -> size) = onGenerator $ dilate $ pure size

closeLuna :: Int -> Image -> Image
closeLuna (variable -> size) = onGenerator $ closing $ pure size

openLuna :: Int -> Image -> Image
openLuna (variable -> size) = onGenerator $ opening $ pure size

premultiplyLuna :: Image -> Image
premultiplyLuna img = (*) `withAlpha` img

unpremultiplyLuna :: Image -> Image
unpremultiplyLuna img = (/) `withAlpha` img

withAlpha :: (A.Exp Double -> A.Exp Double -> A.Exp Double) -> Image -> Image
withAlpha f img = img'
    where (r, g, b, a) = unsafeGetChannels img
          r' = M.zipWith f r a
          g' = M.zipWith f g a
          b' = M.zipWith f b a

          Just view = lookup "rgba" img
          view' = insertChannelFloats view [
                      ("rgba.r", r')
                    , ("rgba.g", g')
                    , ("rgba.b", b')
                    , ("rgba.a", a)
                  ]
          img' = Image.update (const $ Just view') "rgba" img

invertLuna :: Image -> Image
invertLuna = onEachValue invert

colorMatrixLuna :: ColorMatrix Color.RGB Double -> Image -> Image
colorMatrixLuna matrix = onEachRGB (A.lift1 $ (colorMatrix :: ColorMatrix Color.RGB Double -> Color.RGB (A.Exp Double) -> Color.RGB (A.Exp Double)) matrix)

clampLuna :: Double -> Double -> Double -> Double -> Image -> Image
clampLuna (variable -> thLo) (variable -> thHi) (variable -> clampLo) (variable -> clampHi) =
    onEachValue (clamp (Range thLo thHi) (Just $ Range clampLo clampHi))

multiplyLuna :: Color.RGBA Double -> Image -> Image
multiplyLuna (fmap variable -> Color.RGBA r g b a) = onImageRGBA (*r) (*g) (*b) (*a)

gammaLuna :: Color.RGBA Double -> Image -> Image
gammaLuna (fmap variable -> Color.RGBA r g b a) = onImageRGBA (gamma r) (gamma g) (gamma b) (gamma a)

fromPolarMapping :: (Elt a, IsFloating a, Elt e) => CartesianGenerator (Exp a) (Exp e) -> CartesianGenerator (Exp a) (Exp e)
fromPolarMapping (Generator cnv gen) = Generator cnv $ \(Point2 x y) ->
    let Grid cw ch = fmap A.fromIntegral cnv
        radius = (sqrt $ x * x + y * y) / (sqrt $ cw * cw + ch * ch)
        angle  = atan2 y x / (2 * pi)
    in gen (Point2 (angle * cw) (radius * ch))

toPolarMapping :: (Elt a, IsFloating a, Elt e) => CartesianGenerator (Exp a) (Exp e) -> CartesianGenerator (Exp a) (Exp e)
toPolarMapping (Generator cnv gen) = Generator cnv $ \(Point2 angle' radius') ->
    let Grid cw ch = fmap A.fromIntegral cnv
        angle = (angle' / cw) * 2 * pi
        radius = (radius' / ch) * (sqrt $ cw * cw + ch * ch)
    in gen (Point2 (radius * cos angle) (radius * sin angle))

radialBlurLuna :: Int -> Double -> Image -> Image
radialBlurLuna (variable -> size) (variable -> angle) = onEachChannel process
    where kern = monosampler
               $ rotateCenter angle
               $ nearest
               $ rectangle (Grid size 1) 1 0
          process = rasterizer
                  . monosampler
                  . translate (V2 (256) (256))
                  . fromPolarMapping
                  . nearest
                  . normStencil (+) kern (+) 0
                  . monosampler
                  . toPolarMapping
                  . translate (V2 (-256) (-256))
                  . nearest
                  . fromMatrix A.Clamp

histEqLuna :: Int -> Image -> Image
histEqLuna (variable -> bins) img = img'
    where rgb = unsafeGetRGB img
          hsv = M.map Color.liftedConvertColor rgb
          v   = M.map (\(A.unlift -> Color.HSV _ _ v) -> v) hsv
          v'  = M.Delayed $ histeq bins (M.accMatrix v)
          hsv' = M.zipWith (\(A.unlift -> Color.HSV h s _) v -> A.lift $ Color.HSV h s v) hsv v'
          rgb' = M.map Color.liftedConvertColor hsv'
          (r, g, b) = M.unzip3 $ M.map (\(A.unlift -> Color.RGB r g b) -> A.lift (r, g, b)) rgb'

          Just view = lookup "rgba" img

          view' = insertChannelFloats view [
                      ("rgba.r", r)
                    , ("rgba.g", g)
                    , ("rgba.b", b)
                  ]

          img' = Image.update (const $ Just view') "rgba" img

deriving instance Functor A.Boundary

ditherLuna :: A.Boundary Double -> Int -> DiffusionTable Double -> Image -> IO Image
ditherLuna (fmap constantBoundaryWrapper -> boundary) bits table img = do
    let (r, g, b, a) = unsafeGetChannels img
        ditherMethod = dither boundary table bits
    r' <- mutableProcess run ditherMethod r
    g' <- mutableProcess run ditherMethod g
    b' <- mutableProcess run ditherMethod b

    let Just view = lookup "rgba" img
        view' = insertChannelFloats view [
                      ("rgba.r", r')
                    , ("rgba.g", g')
                    , ("rgba.b", b')
                  ]
        img' = Image.update (const $ Just view') "rgba" img

    return img'

orderedDitherLuna :: Int -> Image -> Image
orderedDitherLuna bits = onEachChannel $ bayer bits

constantBoundaryWrapper :: a -> MValue a
constantBoundaryWrapper v = MValue (return v) (const $ return ())

gradeLuna' :: VPS (Color.RGBA Double)
           -> VPS (Color.RGBA Double)
           -> VPS (Color.RGBA Double)
           -> Color.RGBA Double
           -> Color.RGBA Double
           -> Color.RGBA Double
           -> Color.RGBA Double
           -> Image
           -> Image
gradeLuna' (VPS (fmap variable -> Color.RGBA blackpointR blackpointG blackpointB blackpointA))
           (VPS (fmap variable -> Color.RGBA whitepointR whitepointG whitepointB whitepointA))
           (VPS (fmap variable -> Color.RGBA liftR liftG liftB liftA))
           (fmap variable -> Color.RGBA gainR gainG gainB gainA)
           (fmap variable -> Color.RGBA multiplyR multiplyG multiplyB multiplyA)
           (fmap variable -> Color.RGBA offsetR offsetG offsetB offsetA)
           (fmap variable -> Color.RGBA gammaR gammaG gammaB gammaA) =
             onImageRGBA (grade blackpointR whitepointR liftR gainR multiplyR offsetR gammaR)
                         (grade blackpointG whitepointG liftG gainG multiplyG offsetG gammaG)
                         (grade blackpointB whitepointB liftB gainB multiplyB offsetB gammaB)
                         (grade blackpointA whitepointA liftA gainA multiplyA offsetA gammaA)

colorCorrectLuna' :: Color.RGBA Double
                  -> Color.RGBA Double
                  -> Color.RGBA Double
                  -> Color.RGBA Double
                  -> Color.RGBA Double
                  -> Image
                  -> Image
colorCorrectLuna' (fmap variable -> Color.RGBA saturationR saturationG saturationB saturationA)
                  (fmap variable -> Color.RGBA contrastR contrastG contrastB contrastA)
                  (fmap variable -> Color.RGBA gammaR gammaG gammaB gammaA)
                  (fmap variable -> Color.RGBA gainR gainG gainB gainA)
                  (fmap variable -> Color.RGBA offsetR offsetG offsetB offsetA) img =
                      onImageRGBA (colorCorrect contrastR gammaR gainR offsetR)
                                  (colorCorrect contrastG gammaG gainG offsetG)
                                  (colorCorrect contrastB gammaB gainB offsetB)
                                  (colorCorrect contrastA gammaA gainA offsetA) saturated
    where (r, g, b, a) = unsafeGetChannels img
          rgb = unsafeGetRGB img

          rgbRsaturated = M.map (A.lift1 (saturateOnHSV saturationR)) rgb
          rgbGsaturated = M.map (A.lift1 (saturateOnHSV saturationG)) rgb
          rgbBsaturated = M.map (A.lift1 (saturateOnHSV saturationB)) rgb

          saturateOnHSV :: A.Exp Double -> Color.RGB (A.Exp Double) -> Color.RGB (A.Exp Double)
          saturateOnHSV sat pix = Color.toHSV pix & (\(Color.HSV h s v) -> Color.HSV h (s * saturationG) v) & Color.toRGB

          rSaturated = M.map (\(A.unlift -> Color.RGB r _ _) -> r) rgbRsaturated
          gSaturated = M.map (\(A.unlift -> Color.RGB _ g _) -> g) rgbGsaturated
          bSaturated = M.map (\(A.unlift -> Color.RGB _ _ b) -> b) rgbBsaturated

          Just view = lookup "rgba" img

          view' = insertChannelFloats view [
                    ("rgba.r", rSaturated)
                  , ("rgba.g", gSaturated)
                  , ("rgba.b", bSaturated)
                  ]

          saturated = Image.update (const $ Just view') "rgba" img

onImageRGBA :: (A.Exp Double -> A.Exp Double)
            -> (A.Exp Double -> A.Exp Double)
            -> (A.Exp Double -> A.Exp Double)
            -> (A.Exp Double -> A.Exp Double)
            -> Image
            -> Image
onImageRGBA fr fg fb fa img = img'
    where (r, g, b, a) = unsafeGetChannels img
          r' = M.map fr r
          g' = M.map fg g
          b' = M.map fb b
          a' = M.map fa a

          Just view = lookup "rgba" img
          view' = insertChannelFloats view [
                      ("rgba.r", r')
                    , ("rgba.g", g')
                    , ("rgba.b", b')
                    , ("rgba.a", a')
                    ]
          img' = Image.update (const $ Just view') "rgba" img

liftF6 f t1 t2 t3 t4 t5 t6 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    t5' <- t5
    t6' <- t6
    val f <<*>> t1' <<*>> t2' <<*>> t3' <<*>> t4' <<*>> t5' <<*>> t6'

--liftF6 a b c d e f g = do
--    b' <- b
--    c' <- c
--    d' <- d
--    e' <- e
--    f' <- f
--    g' <- g
--    val a <<*>> b' <<*>> c' <<*>> d' <<*>> e' <<*>> f' <<*>> g'

liftF8 a b c d e f g h i = do
    b' <- b
    c' <- c
    d' <- d
    e' <- e
    f' <- f
    g' <- g
    h' <- h
    i' <- i
    val a <<*>> b' <<*>> c' <<*>> d' <<*>> e' <<*>> f' <<*>> g' <<*>> h' <<*>> i'

liftF9 a b c d e f g h i j = do
    b' <- b
    c' <- c
    d' <- d
    e' <- e
    f' <- f
    g' <- g
    h' <- h
    i' <- i
    j' <- j
    val a <<*>> b' <<*>> c' <<*>> d' <<*>> e' <<*>> f' <<*>> g' <<*>> h' <<*>> i' <<*>> j'

liftF12 fun a b c d e f g h i j k l = do
    a' <- a
    b' <- b
    c' <- c
    d' <- d
    e' <- e
    f' <- f
    g' <- g
    h' <- h
    i' <- i
    j' <- j
    k' <- k
    l' <- l
    val fun <<*>> a' <<*>> b' <<*>> c' <<*>> d' <<*>> e' <<*>> f'
            <<*>> g' <<*>> h' <<*>> i' <<*>> j' <<*>> k' <<*>> l'

liftF13 fun a b c d e f g h i j k l m = do
    a' <- a
    b' <- b
    c' <- c
    d' <- d
    e' <- e
    f' <- f
    g' <- g
    h' <- h
    i' <- i
    j' <- j
    k' <- k
    l' <- l
    m' <- m
    val fun <<*>> a' <<*>> b' <<*>> c' <<*>> d' <<*>> e' <<*>> f'
            <<*>> g' <<*>> h' <<*>> i' <<*>> j' <<*>> k' <<*>> l' <<*>> m'

edgeDetectLuna :: Matrix2 Double -> Image -> Image
edgeDetectLuna edgeOperator img = img'
    where alphas = onGenerator (Stencil.stencil (+) (unsafeFromMatrix edgeOperator) (+) 0) img
          (r, g, b, _) = unsafeGetChannels alphas
          alphaSum = M.zipWith3 (\a b c -> a + b + c) r g b
          Just view = lookup "rgba" img
          img' = Image.update (const $ Just $ insertChannelFloats view [("rgba.a", alphaSum)]) "rgba" img

gammaToLinearLuna :: Gamma.Companding a (A.Exp Double) => a -> Image -> Image
gammaToLinearLuna companding = onEachValue $ (Gamma.toLinear companding :: A.Exp Double -> A.Exp Double)

gammaFromLinearLuna :: Gamma.Companding a (A.Exp Double) => a -> Image -> Image
gammaFromLinearLuna companding = onEachValue $ Gamma.fromLinear companding

medianLuna :: Int -> Image -> Image
medianLuna size img = undefined

data InterpolationFilter a = NearestNeighbour
                           | Box
                           | Basic
                           | Triangle
                           | Bell
                           | BSpline
                           | Lanczos a
                           | Polynomial a a
                           | Mitchell
                           | CatmullRom
                           | Gauss a
                           | Dirac a
                           deriving (Show, Functor)

toInterpolator :: (Elt e, IsFloating e) => InterpolationFilter (Exp e) -> DiscreteGenerator (Exp e) -> CartesianGenerator (Exp e) (Exp e)
toInterpolator = \case
    NearestNeighbour -> nearest
    Box              -> interpolator box
    Basic            -> interpolator basic
    Triangle         -> interpolator triangle
    Bell             -> interpolator bell
    BSpline          -> interpolator bspline
    Lanczos a        -> interpolator $ lanczos a
    Polynomial a b   -> interpolator $ polynomial a b
    Mitchell         -> interpolator mitchell
    CatmullRom       -> interpolator catmulRom
    Gauss a          -> interpolator $ gauss a
    Dirac a          -> interpolator $ dirac a

interpolateChannelsLuna :: A.Boundary Double -> InterpolationFilter Double -> Image -> Image
interpolateChannelsLuna (fmap variable -> boundary) (toInterpolator . fmap variable -> interpol) = Image.map (View.map interpolate)
    where interpolate (ChannelFloat name (FlatData mat)) = ChannelGenerator name $ toGen $ mat
          interpolate (ChannelInt   name (FlatData mat)) = ChannelGenerator name $ toGen . M.map A.fromIntegral $ mat
          interpolate (ChannelBit   name (FlatData mat)) = ChannelGenerator name $ toGen . M.map (A.fromIntegral . A.boolToInt) $ mat
          interpolate c@ChannelGenerator{} = c

          toGen = interpol . fromMatrix boundary

toMultisampler :: Grid (Exp Int) -> InterpolationFilter (Exp Double) -> Sampler Double
toMultisampler grid = \case
    NearestNeighbour -> monosampler
    Box              -> multisampler $ normalize $ toMatrix grid box
    Basic            -> multisampler $ normalize $ toMatrix grid basic
    Triangle         -> multisampler $ normalize $ toMatrix grid triangle
    Bell             -> multisampler $ normalize $ toMatrix grid bell
    BSpline          -> multisampler $ normalize $ toMatrix grid bspline
    Lanczos a        -> multisampler $ normalize $ toMatrix grid $ lanczos a
    Polynomial a b   -> multisampler $ normalize $ toMatrix grid $ polynomial a b
    Mitchell         -> multisampler $ normalize $ toMatrix grid mitchell
    CatmullRom       -> multisampler $ normalize $ toMatrix grid catmulRom
    Gauss a          -> multisampler $ normalize $ toMatrix grid $ gauss a
    Dirac a          -> multisampler $ normalize $ toMatrix grid $ dirac a

multisampleChannelsLuna :: Grid Int -> InterpolationFilter Double -> Image -> Image
multisampleChannelsLuna (fmap variable -> grid) (toMultisampler grid . fmap variable -> sampler :: Sampler Double) = Image.map (View.map multisample)
    where multisample (ChannelGenerator name gen) = ChannelFloat name $ FlatData . rasterizer . sampler $ gen
          --                                                            FIXME[MM]: ^ we don't want this here,
          --                                                                         but ChannelGenerator requires ContinousGenerator :/
          multisample channel                     = channel

-- FIXME[MM]: will remove the whole view if removing fails - it should somehow propagate the error
removeChannelLuna :: String -> String -> Image -> Image
removeChannelLuna viewName channelName = Image.update f viewName
    where f view = case View.remove channelName view of
                  Left _ -> Nothing
                  Right v -> Just v

getChannelLuna :: String -> String -> Image -> Image.Result (Maybe Channel)
getChannelLuna viewName channelName img = case Image.lookup viewName img of
    Just view -> View.get view channelName
    _         -> Left $ Image.ViewLookupError viewName

insertChannelLuna :: String -> Channel -> Image -> Image
insertChannelLuna viewName chan = Image.update f viewName
    where f = Just . View.append chan

type ControlPoint2 a = ( VPS (Point2 a)
                       , VPS (Maybe (Point2 a))
                       , VPS (Maybe (Point2 a))
                       )

type Path2 a = ( VPS Bool
               , VPS [VPS (ControlPoint2 a)]
               )

type Shape2 a = [VPS (Path2 a)]

type Mask2 a = ( VPS (Path2 a)
               , VPS (Maybe (Path2 a))
               )

unpackLunaVar :: VPS a -> a
unpackLunaVar (Value (Pure (Safe a))) = a

unpackLunaList :: [VPS a] -> [a]
unpackLunaList = fmap unpackLunaVar

convertControlPoint :: ControlPoint2 a -> ControlPoint a
convertControlPoint (unpackLunaVar -> a, unpackLunaVar -> b, unpackLunaVar -> c) = ControlPoint a b c

convertPath :: Path2 a -> Path a
convertPath (unpackLunaVar -> a, unpackLunaList.unpackLunaVar -> b) = Path a (fmap convertControlPoint b)

convertShape :: Shape2 a -> GShape.Shape a
convertShape (unpackLunaList -> a) = GShape.Shape (fmap convertPath a)

convertMask :: Mask2 a -> Mask.Mask a
convertMask (unpackLunaVar -> a, unpackLunaVar -> b) = Mask.Mask (convertPath a) (fmap convertPath b)

rasterizeMaskLuna :: (Real a, Fractional a) => Int -> Int -> Mask2 a -> Image
rasterizeMaskLuna w h (convertMask -> m) = matrixToImage $ rasterizeMask w h m

testF5 :: Color.RGBA Double
       -> Color.RGBA Double
       -> Color.RGBA Double
       -> Color.RGBA Double
       -> Image
       -> Int
testF5 _ _ _ _ _ = 5
