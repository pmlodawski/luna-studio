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
    , variable
) where

import qualified Codec.Picture.Png                 as Juicy
import qualified Codec.Picture.Types               as Juicy
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Array.Sugar as A
import           Data.Array.Accelerate.CUDA
import qualified Data.Array.Accelerate.IO          as A
import           Data.Bool
import           Data.Char                         (toLower)
import           Data.Maybe
import qualified Data.Vector.Storable              as SV
import           Math.Coordinate.Cartesian
import           Math.Space.Space
import           Math.Metric
import           Linear                            (V2(..))
import           System.FilePath                   as FilePath

import qualified Flowbox.Graphics.Color.Color                         as Color
import qualified Flowbox.Graphics.Color.Companding                    as Gamma
import           Flowbox.Graphics.Composition.Dither
import           Flowbox.Geom2D.Accelerate.CubicBezier
import           Flowbox.Geom2D.Accelerate.CubicBezier.Solve          as CubicSolveAcc
import           Flowbox.Geom2D.ControlPoint
import           Flowbox.Geom2D.CubicBezier
import           Flowbox.Geom2D.Path
import           Flowbox.Geom2D.Rectangle
import qualified Flowbox.Geom2D.Shape                                 as GShape
import qualified Flowbox.Geom2D.Mask as Mask
import           Flowbox.Geom2D.Rasterizer
import           Flowbox.Graphics.Composition.Filter
import           Flowbox.Graphics.Composition.Filter                  as Conv
import           Flowbox.Graphics.Composition.Generator.Gradient
import           Flowbox.Graphics.Composition.Keyer
import qualified Flowbox.Graphics.Composition.EdgeBlur                as EB
import           Flowbox.Graphics.Shader.Matrix                       as Shader
import           Flowbox.Graphics.Composition.Generator.Noise.Billow
import           Flowbox.Graphics.Composition.Generator.Noise.Perlin
import           Flowbox.Graphics.Shader.Pipe
import           Flowbox.Graphics.Shader.Rasterizer
import           Flowbox.Graphics.Shader.Sampler      as Shader
import           Flowbox.Graphics.Composition.Generator.Shape
import           Flowbox.Graphics.Shader.Stencil      as Stencil
import           Flowbox.Graphics.Shader.Shader       as Shader
import           Flowbox.Graphics.Composition.Transform as Shader
import           Flowbox.Graphics.Composition.Histogram
import qualified Flowbox.Graphics.Composition.Generator.Raster                  as Raster
import           Flowbox.Graphics.Image.Channel                       as Channel
import           Flowbox.Graphics.Composition.Color
import           Flowbox.Graphics.Image.Image                         as Image
import           Flowbox.Graphics.Image.Error                         as Image
import           Flowbox.Graphics.Image.IO.ImageMagick                (loadImage, saveImage)
import           Flowbox.Graphics.Image.IO.OpenEXR                    (readFromEXR)
import           Flowbox.Graphics.Composition.Merge                         (AlphaBlend(..))
import qualified Flowbox.Graphics.Composition.Merge                   as Merge
import           Flowbox.Graphics.Image.View                          as View
import           Flowbox.Graphics.Utils.Utils
import           Flowbox.Math.Matrix                                  as M
import           Flowbox.Prelude                                      as P hiding (lookup)
import           Flowbox.Math.Function.Accelerate.BSpline             as BSpline
import qualified Flowbox.Math.Function.CurveGUI                       as CurveGUI

import Luna.Target.HS (Pure (..), Safe (..), Value (..), autoLift, autoLift1, fromValue, val)
import Control.PolyApplicative ((<<*>>))


data SkewOrder = SkewXY | SkewYX

data Skew a = Skew { _skewPoint :: Point2 a
                   , _skewOrder :: SkewOrder
                   }

data Transform a = Transform { _translate :: Point2 a
                             , _rotate    :: a
                             , _scale     :: Point2 a
                             , _skew      :: Skew a
                             , _center    :: Point2 a
                             }

pattern VPS x = Value (Pure (Safe x))
type VPS x = Value Pure Safe x

type ColorD = Color.RGBA Double
pattern ColorD r g b a = Color.RGBA r g b a
type Color5 = (VPS ColorD, VPS ColorD, VPS ColorD, VPS ColorD, VPS ColorD)

-- == LOAD / SAVE

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


-- == HELPERS

onEach :: (A.Exp Double -> A.Exp Double) -> Image -> Image
onEach f = Image.map (View.map $ Channel.unsafeMap (Channel.FunDouble f))

onEachRGBA :: (A.Exp Double -> A.Exp Double)
           -> (A.Exp Double -> A.Exp Double)
           -> (A.Exp Double -> A.Exp Double)
           -> (A.Exp Double -> A.Exp Double)
           -> Image
           -> Image
onEachRGBA fr fg fb fa img = Image.appendMultiToPrimary [r,g,b,a] img
    where r = updateChan fr "rgba.r"
          g = updateChan fg "rgba.g"
          b = updateChan fb "rgba.b"
          a = updateChan fa "rgba.a"
          updateChan f = Channel.unsafeMap (Channel.FunDouble f) . getChan
          getChan chanName = let Right (Just chan) = Image.getFromPrimary chanName img in chan

onEachColorRGB :: (A.Exp (Color.RGB Double) -> A.Exp (Color.RGB Double)) -> Image -> Image
onEachColorRGB f img = img'
    where rgb = unsafeGetRGB img
          Right view = lookupPrimary img
          rgb' = M.map f rgb

          unzipRGB = M.unzip3 . M.map (\(A.unlift -> Color.RGB x y z) -> A.lift (x, y, z))

          (r', g', b') = unzipRGB rgb'

          view' = insertChannelFloats view [
                      ("rgba.r", r')
                    , ("rgba.g", g')
                    , ("rgba.b", b')
                  ]

          img' = Image.insertPrimary view' img

onEachChannel :: (Channel -> Channel) -> Image -> Image
onEachChannel f = Image.map $ View.map f


-- == COMPO

--defocus :: Int -> Image -> Image
--defocus size = onEachChannel process
--    where kernel = ellipse (pure $ variable size) 1 (0 :: A.Exp Double)
--          process = rasterizer . normStencil (+) kernel (+) 0 . fromMatrix A.Clamp

--motionBlur :: Int -> Double -> Image -> Image
--motionBlur size angle = onEachChannel process
--    where kernel = monosampler
--                 $ rotateCenter (variable angle)
--                 $ nearest
--                 $ rectangle (Grid (variable size) 1) 1 0
--          process = rasterizer . normStencil (+) kernel (+) 0 . fromMatrix A.Clamp

edgeBlur :: View.Name -> Channel.Name -> Int -> Double -> Image -> Image
edgeBlur viewName channelName kernelSize edgeMultiplier image =
    let --(r, g, b, a) = unsafeGetChannels image
        Right ch = getChannelLuna viewName channelName image
        Just (ChannelFloat _ (MatrixData channelMat)) = ch
        maskEdges = EB.edges (variable edgeMultiplier) channelMat -- TODO: channel choice
        blurFunc = EB.maskBlur EB.Gauss (variable kernelSize) maskEdges
      in onEachChannel blurFunc image


-- rotateCenter :: (Elt a, IsFloating a) => Exp a -> CartesianShader (Exp a) b -> CartesianShader (Exp a) b
--rotateCenter phi = canvasT (fmap A.ceiling . rotate phi . asFloating) . onCenter (rotate phi)

--bilateral :: Double
--          -> Double
--          -> Int
--          -> Image
--          -> Image
--bilateral psigma csigma (variable -> size) = onEachChannel process
--    where p = pipe A.Clamp
--          spatial :: Shader (Point2 (Exp Int)) (Exp Double)
--          spatial = Shader (pure $ variable size) $ \(Point2 x y) ->
--              let dst = sqrt . A.fromIntegral $ (x - size `div` 2) * (x - size `div` 2) + (y - size `div` 2) * (y - size `div` 2)
--              in apply (gauss $ variable psigma) dst
--          domain center neighbour = apply (gauss $ variable csigma) (abs $ neighbour - center)
--          process = rasterizer . (id `p` bilateralStencil (+) spatial domain (+) 0 `p` id) . fromMatrix A.Clamp

offsetLuna :: Color.RGBA Double -> Image -> Image
offsetLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (offset r) (offset g) (offset b) id -- (offset a)

contrastLuna :: Color.RGBA Double -> Image -> Image
contrastLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (contrast r) (contrast g) (contrast b) id -- (contrast a)

exposureLuna :: Color.RGBA Double -> Color.RGBA Double -> Image -> Image
exposureLuna (fmap variable -> Color.RGBA blackpointR blackpointG blackpointB blackpointA)
             (fmap variable -> Color.RGBA exR exG exB exA) =
                 onEachRGBA (exposure blackpointR exR)
                            (exposure blackpointG exG)
                            (exposure blackpointB exB)
                            id -- (exposure blackpointA exA)

gradeLuna :: VPS Double -> VPS Double -> VPS Double -> Double -> Double -> Double -> Double -> Image -> Image
gradeLuna (VPS (variable -> blackpoint))
          (VPS (variable -> whitepoint))
          (VPS (variable -> lift))
          (variable -> gain)
          (variable -> multiply')
          (variable -> offset')
          (variable -> gamma') =
            onEach $ grade blackpoint whitepoint lift gain multiply' offset' gamma'

saturateLuna :: Color.RGBA Double -> Image -> Image
saturateLuna (fmap variable -> Color.RGBA saturationR saturationG saturationB saturationA) img = saturated
    where rgb = unsafeGetRGB img

          rgbRsaturated = M.map (A.lift1 (saturateOnHSV saturationR)) rgb
          rgbGsaturated = M.map (A.lift1 (saturateOnHSV saturationG)) rgb
          rgbBsaturated = M.map (A.lift1 (saturateOnHSV saturationB)) rgb

          saturateOnHSV :: A.Exp Double -> Color.RGB (A.Exp Double) -> Color.RGB (A.Exp Double)
          saturateOnHSV sat pix = Color.toHSL pix & (\(Color.HSL h s l) -> Color.HSL h (s * sat) l) & Color.toRGB

          rSaturated = M.map (\(A.unlift -> Color.RGB r _ _) -> r) rgbRsaturated
          gSaturated = M.map (\(A.unlift -> Color.RGB _ g _) -> g) rgbGsaturated
          bSaturated = M.map (\(A.unlift -> Color.RGB _ _ b) -> b) rgbBsaturated

          Right view = lookupPrimary img

          view' = insertChannelFloats view [
                    ("rgba.r", rSaturated)
                  , ("rgba.g", gSaturated)
                  , ("rgba.b", bSaturated)
                  ]

          saturated = Image.insertPrimary view' img

posterizeLuna :: Double -> Image -> Image
posterizeLuna (variable -> colors) = onEach $ posterize colors

loadImageLuna :: FilePath -> IO Image
loadImageLuna path = do
    (r, g, b, a) <- testLoadRGBA path
    let view = insertChannelFloats (View.emptyDefault) [
                   ("rgba.r", r)
                 , ("rgba.g", g)
                 , ("rgba.b", b)
                 , ("rgba.a", a)
               ]
        image = singleton view
    return image

insertChannelFloats :: View -> [(String, Matrix2 Double)] -> View
insertChannelFloats view chans = foldr f view chans
    where f (name, chan) acc = View.append (ChannelFloat name . MatrixData $ chan) acc

saveImageLuna :: FilePath -> Image -> IO Image
saveImageLuna path img = do
    let (r, g, b, a) = unsafeGetChannels img
    testSaveRGBA path r g b a
    return img

keyer' :: (A.Exp (Color.RGB Double) -> A.Exp Double) -> Image -> Image
keyer' f img = img'
    where rgb = unsafeGetRGB img
          Right view = lookupPrimary img
          alpha = M.map f rgb

          view' = insertChannelFloats view [("rgba.a", alpha)]

          img' = Image.insertPrimary view' img

unsafeGetRGB :: Image -> M.Matrix2 (Color.RGB Double)
unsafeGetRGB img = rgb
    where (r, g, b, _) = unsafeGetChannels img

          rgb = M.zipWith3 (\x y z -> A.lift $ Color.RGB x y z) r g b

unsafeGetChannels :: Image -> (M.Matrix2 Double, M.Matrix2 Double, M.Matrix2 Double, M.Matrix2 Double)
unsafeGetChannels img = (r, g, b, a)
    where Right view = lookupPrimary img
          Right (Just (ChannelFloat _ (asMatrixData -> MatrixData r))) = View.get view "rgba.r"
          Right (Just (ChannelFloat _ (asMatrixData -> MatrixData g))) = View.get view "rgba.g"
          Right (Just (ChannelFloat _ (asMatrixData -> MatrixData b))) = View.get view "rgba.b"
          Right (Just (ChannelFloat _ (asMatrixData -> MatrixData a))) = View.get view "rgba.a"

keyerLuna :: KeyerMode -> Double -> Double -> Double -> Double -> Image -> Image
keyerLuna mode (variable -> a) (variable -> b) (variable -> c) (variable -> d) img =
    keyer' (keyer mode (A.lift $ (a, b, c, d))) img

differenceKeyer' :: (A.Exp (Color.RGB Double) -> A.Exp (Color.RGB Double) -> A.Exp Double) -> Image -> Image -> Image
differenceKeyer' f background foreground = img'
    where backgroundRGB = unsafeGetRGB background
          foregroundRGB = unsafeGetRGB foreground

          alpha = M.map (A.uncurry f) $ M.zip backgroundRGB foregroundRGB

          Right view = lookupPrimary foreground
          view' = insertChannelFloats view [("rgba.a", alpha)]

          img' = Image.insertPrimary view' foreground

differenceKeyerLuna :: Double -> Double -> Image -> Image -> Image
differenceKeyerLuna (variable -> offset) (variable -> gain) background foreground = img'
    where diff = differenceKeyer offset gain
          img' = differenceKeyer' diff background foreground

--cornerPinLuna :: Double -> Double
--              -> Double -> Double
--              -> Double -> Double
--              -> Double -> Double
--              -> Image
--              -> Image
--cornerPinLuna (variable -> p1x) (variable -> p1y)
--              (variable -> p2x) (variable -> p2y)
--              (variable -> p3x) (variable -> p3y)
--              (variable -> p4x) (variable -> p4y) img = img'
--    where img' = onEachChannel process img
--          process = rasterizer . monosampler . cornerPin (p1, p2, p3, p4) . nearest . fromMatrix (A.Constant 0)
--          p1 = Point2 p1x p1y
--          p2 = Point2 p2x p2y
--          p3 = Point2 p3x p3y
--          p4 = Point2 p4x p4y

--gaussianLuna :: Int -> Image -> Image
--gaussianLuna (variable -> kernelSize) img = img'
--    where img' = onEachChannel process img
--          hmat = id M.>-> normalize $ toMatrix (Grid 1 kernelSize) $ gauss 1.0
--          vmat = id M.>-> normalize $ toMatrix (Grid kernelSize 1) $ gauss 1.0
--          p = pipe A.Clamp
--          process x = rasterizer $ id `p` Conv.filter 1 vmat `p` Conv.filter 1 hmat `p` id $ fromMatrix A.Clamp x

--laplacianLuna :: Int -> Double -> Double -> Image -> Image
--laplacianLuna (variable -> kernSize) (variable -> crossVal) (variable -> sideVal) img = img'
--    where img' = onEachChannel process img
--          process x = rasterizer $ id `p` Conv.filter 1 flt `p` id $ fromMatrix A.Clamp x
--          flt = laplacian crossVal sideVal $ pure kernSize
--          p = pipe A.Clamp

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
                      Shader (Point2 (Exp Double)) (Exp Double) -> e -> e -> Image
gradientLuna gradient (variable -> width) (variable -> height) = channelToImageRGBA grad
    where grad = rasterizer $ monosampler $ gradientShader

          gradientShader = scale (Grid width height) $ Shader.translate (V2 0.5 0.5) $ mapper gray gradient
          gray   = [Tick 0.0 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Double Double Double]

          weightFun tickPos val1 weight1 val2 weight2 = mix tickPos val1 val2
          mapper = flip colorMapper weightFun

channelToImageRGBA :: Matrix2 Double -> Image
channelToImageRGBA m = image
    where image = singleton view
          view = insertChannelFloats (View.emptyDefault) [
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
                   CartesianShader (Exp a) (Exp Double) -> e -> e -> Image
noiseLuna noise (variable -> width) (variable -> height) = channelToImageRGBA noise'
    where noise' = rasterizer $ monosampler $ noiseShader

          noiseShader = scale (Grid width height) noise

translateLuna :: Int -> Int -> Image -> Image
translateLuna (A.fromIntegral . variable -> x) (A.fromIntegral . variable -> y) = onEachChannel f
    where v = V2 x (-y)
          f = \case
              (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform p shader) zeData
              (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt name   $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform p shader) zeData
              (Channel.asContinuous -> ChannelBit   name zeData) -> ChannelBit name   $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform p shader) zeData
          mask = Nothing
          p :: Point2 (Exp Double) -> Point2 (Exp Double)
          p pt = Shader.translate (handle pt) pt
          handle :: Point2 (Exp Double) -> V2 (Exp Double)
          handle pt = case mask of
              Nothing      -> v
              --TODO[KM]: handle the mask properly (aka. get rid of that ugly pattern match) and uncomment the other case option
              _ -> v
              --Just (VPS m) -> let
              --        Right rgba = Image.lookupPrimary m
              --        unpackMat (Right (Just (ChannelFloat _ (asMatrixData -> MatrixData c)))) = c
              --        m' = unpackMat $ View.get rgba "rgba.r"
              --        Shader _ str = Shader.nearest $ Shader.fromMatrix (A.Constant (0 :: Exp Double)) $ m'
              --        mult :: Point2 (Exp Double) -> Exp Double -> Exp Double
              --        mult pt x = str pt * x
              --    in (fmap (mult pt) v)

--translateLuna :: Int -> Int -> Image -> Image
--translateLuna (variable -> x) (variable -> y) = onEachMatrix process process process process
--    where v = V2 x (-y)
--          mask = Nothing
--          process :: Matrix2 Double -> Matrix2 Double
--          process = rasterizer . t . gen
--          gen = fromMatrix (A.Constant (0 :: Exp Double))
--          t :: DiscreteShader (Exp Double) -> DiscreteShader (Exp Double)
--          t = Shader.transform p
--          p :: Point2 (Exp Int) -> Point2 (Exp Int)
--          p pt = translate (handle pt) pt
--          handle pt = case mask of
--              Nothing      -> v
--              Just (VPS m) -> let
--                      Right rgba = Image.lookupPrimary m
--                      unpackMat (Right (Just (ChannelFloat _ (asMatrixData -> MatrixData c)))) = c
--                      m' = unpackMat $ View.get rgba "rgba.r"
--                      Shader _ str = gen m'
--                      mult pt x = A.round $ (str pt) * A.fromIntegral x
--                  in (fmap (mult pt) v)

--turnCenter :: (Elt a, IsFloating a) => Exp a -> CartesianShader (Exp a) b -> CartesianShader (Exp a) b
--turnCenter = onCenter . rotate

--turnCenterLuna :: Double -> Image -> Image
--turnCenterLuna (variable -> angle) = onEachChannel $ rasterizer . monosampler . turnCenter angle . nearest . fromMatrix (A.Constant 0)

--scaleToLuna :: A.Boundary (A.Exp Double) -> Int -> Int -> Image -> Image
--scaleToLuna boundary (variable -> x) (variable -> y) = onEachChannel $ rasterizer . monosampler . foo
--    where foo :: Matrix2 Double -> ContinuousShader (A.Exp Double)
--          foo = scale (Grid x y) . nearest . fromMatrix boundary

--scaleLuna :: A.Boundary (A.Exp Double) -> Double -> Double -> Image -> Image
--scaleLuna boundary (variable -> x) (variable -> y) = onEachChannel $ rasterizer . monosampler . canvasT f . scale (V2 x y) . interpolator (Conv.catmulRom) . fromMatrix boundary
--    where f = fmap A.truncate . scale (V2 x y) . asFloating
--scaleLuna :: Double -> Double -> Maybe (VPS Image) -> Image -> Image
--scaleLuna :: Bool -> Double -> Double -> Image -> Image
--scaleLuna centered (variable -> x) (variable -> y) = onEachMatrix process process process process
--    where v = V2 x y
--          mask = Nothing
--          process :: Matrix2 Double -> Matrix2 Double
--          process = rasterizer . monosampler . t . interpolator (Conv.catmulRom) . gen
--          --f = canvasT $ fmap A.truncate . scale (V2 x y) . asFloating
--          gen = fromMatrix (A.Constant (0 :: Exp Double))
--          t :: CartesianShader (Exp Double) (Exp Double) -> CartesianShader (Exp Double) (Exp Double)
--          t = bool tp (onCenter tp) centered
--          tp = Shader.transform p
--          p :: Point2 (Exp Double) -> Point2 (Exp Double)
--          p pt = scale (handle pt) pt
--          handle :: Point2 (Exp Double) -> V2 (Exp Double)
--          handle pt = case mask of
--              Nothing      -> v
--              Just (VPS m) -> let
--                      Right rgba = Image.lookupPrimary m
--                      unpackMat (Right (Just (ChannelFloat _ (asMatrixData -> MatrixData c)))) = c
--                      m' = unpackMat $ View.get rgba "rgba.r"
--                      Shader _ str = gen m'
--                      mult :: Point2 (Exp Double) -> Exp Double -> Exp Double
--                      mult pt x = str (fmap A.floor pt) * x
--                  in (fmap (mult pt) v)

transformLuna :: Transform Double -> Image -> Image
transformLuna _ img = img

cropLuna :: Rectangle Double -> Image -> Image
cropLuna _ img = img

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
    onEachColorRGB $ A.lift1 (hsvTool (A.lift $ Range hueRangeStart hueRangeEnd) hueRotation hueRolloff
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
                    img' = Image.insertPrimary view' img1
          Right view = lookupPrimary img1
          (r1, g1, b1, a1) = unsafeGetChannels img1 & over each (fromMatrix (A.Constant 0))
          (r2, g2, b2, a2) = unsafeGetChannels img2 & over each (fromMatrix (A.Constant 0))

onShader f img = img'
    where (r, g, b, a) = unsafeGetChannels img & over each (rasterizer . f . fromMatrix (A.Constant 0))
          Right view = lookupPrimary img
          view' = insertChannelFloats view [
                      ("rgba.r", r)
                    , ("rgba.g", g)
                    , ("rgba.b", b)
                    , ("rgba.a", a)
                  ]
          img' = Image.insertPrimary view' img

erodeLuna :: Int -> Image -> Image
erodeLuna (variable -> size) = onShader $ erode $ pure size

dilateLuna :: Int -> Image -> Image
dilateLuna (variable -> size) = onShader $ dilate $ pure size

closeLuna :: Int -> Image -> Image
closeLuna (variable -> size) = onShader $ closing $ pure size

openLuna :: Int -> Image -> Image
openLuna (variable -> size) = onShader $ opening $ pure size

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

          Right view = lookupPrimary img
          view' = insertChannelFloats view [
                      ("rgba.r", r')
                    , ("rgba.g", g')
                    , ("rgba.b", b')
                    , ("rgba.a", a)
                  ]
          img' = Image.insertPrimary view' img

invertLuna :: Image -> Image
invertLuna = onEachRGBA invert invert invert id

colorMatrixLuna :: ColorMatrix Color.RGB Double -> Image -> Image
colorMatrixLuna matrix = onEachColorRGB (A.lift1 $ (colorMatrix :: ColorMatrix Color.RGB Double -> Color.RGB (A.Exp Double) -> Color.RGB (A.Exp Double)) matrix)

clampLuna :: (VPS Double, VPS Double) -> Maybe (VPS Double, VPS Double) -> Image -> Image
clampLuna (VPS (variable -> thLo), VPS (variable -> thHi)) clamps =
    case clamps of
        Just (VPS clampLo, VPS clampHi) -> onEach $ clamp (Range thLo thHi) $ Just $ Range (variable clampLo) (variable clampHi)
        _                               -> onEach $ clamp (Range thLo thHi) Nothing

multiplyLuna :: Color.RGBA Double -> Image -> Image
multiplyLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (*r) (*g) (*b) id -- (*a)

gammaLuna :: Color.RGBA Double -> Image -> Image
gammaLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (gamma r) (gamma g) (gamma b) id -- (gamma a)

fromPolarMapping :: (Elt a, IsFloating a, Elt e) => CartesianShader (Exp a) (Exp e) -> CartesianShader (Exp a) (Exp e)
fromPolarMapping (Shader cnv gen) = Shader cnv $ \(Point2 x y) ->
    let Grid cw ch = fmap A.fromIntegral cnv
        radius = (sqrt $ x * x + y * y) / (sqrt $ cw * cw + ch * ch)
        angle  = atan2 y x / (2 * pi)
    in gen (Point2 (angle * cw) (radius * ch))

toPolarMapping :: (Elt a, IsFloating a, Elt e) => CartesianShader (Exp a) (Exp e) -> CartesianShader (Exp a) (Exp e)
toPolarMapping (Shader cnv gen) = Shader cnv $ \(Point2 angle' radius') ->
    let Grid cw ch = fmap A.fromIntegral cnv
        angle = (angle' / cw) * 2 * pi
        radius = (radius' / ch) * (sqrt $ cw * cw + ch * ch)
    in gen (Point2 (radius * cos angle) (radius * sin angle))

--radialBlurLuna :: Int -> Double -> Image -> Image
--radialBlurLuna (variable -> size) (variable -> angle) = onEachChannel process
--    where kern = monosampler
--               $ rotateCenter angle
--               $ nearest
--               $ rectangle (Grid size 1) 1 0
--          process = rasterizer
--                  . monosampler
--                  . foo
--          foo :: Matrix2 Double -> ContinuousShader (Exp Double)
--          foo     = translate (V2 (256) (256))
--                  . fromPolarMapping
--                  . nearest
--                  . normStencil (+) kern (+) 0
--                  . monosampler
--                  . (toPolarMapping :: ContinuousShader (Exp Double) -> ContinuousShader (Exp Double))
--                  . translate (V2 (-256) (-256))
--                  . nearest
--                  . fromMatrix A.Clamp

histEqLuna :: Int -> Image -> Image
histEqLuna (variable -> bins) img = img'
    where rgb = unsafeGetRGB img
          hsv = M.map Color.liftedConvertColor rgb
          v   = M.map (\(A.unlift -> Color.HSV _ _ v) -> v) hsv
          v'  = M.Delayed $ histeq bins (M.accMatrix v)
          hsv' = M.zipWith (\(A.unlift -> Color.HSV h s _) v -> A.lift $ Color.HSV h s v) hsv v'
          rgb' = M.map Color.liftedConvertColor hsv'
          (r, g, b) = M.unzip3 $ M.map (\(A.unlift -> Color.RGB r g b) -> A.lift (r, g, b)) rgb'

          Right view = lookupPrimary img

          view' = insertChannelFloats view [
                      ("rgba.r", r)
                    , ("rgba.g", g)
                    , ("rgba.b", b)
                  ]

          img' = Image.insertPrimary view' img

deriving instance Functor A.Boundary

ditherLuna :: A.Boundary Double -> Int -> DiffusionTable Double -> Image -> IO Image
ditherLuna (fmap constantBoundaryWrapper -> boundary) bits table img = do
    let (r, g, b, a) = unsafeGetChannels img
        ditherMethod = dither boundary table bits
    r' <- mutableProcess run ditherMethod r
    g' <- mutableProcess run ditherMethod g
    b' <- mutableProcess run ditherMethod b

    let Right view = lookupPrimary img
        view' = insertChannelFloats view [
                      ("rgba.r", r')
                    , ("rgba.g", g')
                    , ("rgba.b", b')
                  ]
        img' = Image.insertPrimary view' img

    return img'

--orderedDitherLuna :: Int -> Image -> Image
--orderedDitherLuna bits = onEachChannel $ bayer bits

constantBoundaryWrapper :: a -> MValue a
constantBoundaryWrapper v = MValue (return v) (const $ return ())

type Handle = (VPS Int, VPS Double, VPS Double)
type GUIControlPoint a = (VPS (Point2 a), VPS Handle, VPS Handle)
type GUICurve a = [(VPS (GUIControlPoint a))]

convertHandle :: Handle -> CurveGUI.Handle
convertHandle (unpackLunaVar -> t, unpackLunaVar -> w, unpackLunaVar -> a) =
    case t of
        0 -> CurveGUI.NonLinear w a
        1 -> case a > 0 of
                True  -> CurveGUI.Vertical w CurveGUI.Up
                False -> CurveGUI.Vertical w CurveGUI.Down
        2 -> CurveGUI.Linear

convertGUIControlPoint :: GUIControlPoint a -> CurveGUI.ControlPoint a
convertGUIControlPoint (unpackLunaVar -> p, unpackLunaVar -> hIn, unpackLunaVar -> hOut) =
    CurveGUI.ControlPoint p (convertHandle hIn) (convertHandle hOut)

convertGUICurve :: GUICurve a -> CurveGUI.Curve a
convertGUICurve (unpackLunaList -> c) = CurveGUI.BezierCurve (fmap convertGUIControlPoint c)

hueCorrectLuna :: VPS (GUICurve Double) -> VPS (GUICurve Double) ->
                  VPS (GUICurve Double) -> VPS (GUICurve Double) -> VPS (GUICurve Double) ->
                  GUICurve Double -> GUICurve Double -> GUICurve Double ->
                  -- GUICurve Double -> sat_thrsh will be added later
                  -- sat_thrsh affects only r,g,b and lum parameters
                  Image -> Image
hueCorrectLuna (VPS (convertGUICurve-> lum)) (VPS (convertGUICurve -> sat))
               (VPS (convertGUICurve -> r)) (VPS (convertGUICurve-> g))
               (VPS (convertGUICurve -> b)) (convertGUICurve -> rSup)
               (convertGUICurve -> gSup) (convertGUICurve-> bSup) img
                    = onEachColorRGB (hueCorrect (CurveGUI.convertToBSpline lum)
                                                 (CurveGUI.convertToBSpline sat)
                                                 (CurveGUI.convertToBSpline r)
                                                 (CurveGUI.convertToBSpline g)
                                                 (CurveGUI.convertToBSpline b)
                                                 (CurveGUI.convertToBSpline rSup)
                                                 (CurveGUI.convertToBSpline gSup)
                                                 (CurveGUI.convertToBSpline bSup)
                                     ) img

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
             onEachRGBA (grade blackpointR whitepointR liftR gainR multiplyR offsetR gammaR)
                        (grade blackpointG whitepointG liftG gainG multiplyG offsetG gammaG)
                        (grade blackpointB whitepointB liftB gainB multiplyB offsetB gammaB)
                        id -- (grade blackpointA whitepointA liftA gainA multiplyA offsetA gammaA)

colorCorrectLuna' :: Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                  -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                  -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                  -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                  -> Image
                  -> Image

colorCorrectLuna' ( VPS (fmap variable -> ColorD masterSaturationR masterSaturationG masterSaturationB masterSaturationA)
                  , VPS (fmap variable -> ColorD masterContrastR masterContrastG masterContrastB masterContrastA)
                  , VPS (fmap variable -> ColorD masterGammaR masterGammaG masterGammaB masterGammaA)
                  , VPS (fmap variable -> ColorD masterGainR masterGainG masterGainB masterGainA)
                  , VPS (fmap variable -> ColorD masterOffsetR masterOffsetG masterOffsetB masterOffsetA)
                  )
                  ( VPS (fmap variable -> ColorD shadowsSaturationR shadowsSaturationG shadowsSaturationB shadowsSaturationA)
                  , VPS (fmap variable -> ColorD shadowsContrastR shadowsContrastG shadowsContrastB shadowsContrastA)
                  , VPS (fmap variable -> ColorD shadowsGammaR shadowsGammaG shadowsGammaB shadowsGammaA)
                  , VPS (fmap variable -> ColorD shadowsGainR shadowsGainG shadowsGainB shadowsGainA)
                  , VPS (fmap variable -> ColorD shadowsOffsetR shadowsOffsetG shadowsOffsetB shadowsOffsetA)
                  )
                  ( VPS (fmap variable -> ColorD midtonesSaturationR midtonesSaturationG midtonesSaturationB midtonesSaturationA)
                  , VPS (fmap variable -> ColorD midtonesContrastR midtonesContrastG midtonesContrastB midtonesContrastA)
                  , VPS (fmap variable -> ColorD midtonesGammaR midtonesGammaG midtonesGammaB midtonesGammaA)
                  , VPS (fmap variable -> ColorD midtonesGainR midtonesGainG midtonesGainB midtonesGainA)
                  , VPS (fmap variable -> ColorD midtonesOffsetR midtonesOffsetG midtonesOffsetB midtonesOffsetA)
                  )
                  ( VPS (fmap variable -> ColorD highlightsSaturationR highlightsSaturationG highlightsSaturationB highlightsSaturationA)
                  , VPS (fmap variable -> ColorD highlightsContrastR highlightsContrastG highlightsContrastB highlightsContrastA)
                  , VPS (fmap variable -> ColorD highlightsGammaR highlightsGammaG highlightsGammaB highlightsGammaA)
                  , VPS (fmap variable -> ColorD highlightsGainR highlightsGainG highlightsGainB highlightsGainA)
                  , VPS (fmap variable -> ColorD highlightsOffsetR highlightsOffsetG highlightsOffsetB highlightsOffsetA)
                  )
                  img =
                      onEachRGBA (correct' correctMasterR correctShadowsR correctMidtonesR correctHighlightsR)
                                 (correct' correctMasterG correctShadowsG correctMidtonesG correctHighlightsG)
                                 (correct' correctMasterB correctShadowsB correctMidtonesB correctHighlightsB)
                                 id -- (colorCorrect contrastA gammaA gainA offsetA) saturated
                                 saturated
    where
          curveShadows    = A.lift $ CubicBezier (Point2 (0::Double) 1) (Point2 0.03 1) (Point2 0.06 0) (Point2 0.09 0) :: Exp (CubicBezier Double)
          curveHighlights = A.lift $ CubicBezier (Point2 0.5 (0::Double)) (Point2 (2/3) 0) (Point2 (5/6) 1) (Point2 1 1) :: Exp (CubicBezier Double)
          strShadows x    = A.cond (x A.<=* 0) 1
                          $ A.cond (x A.>=* 0.09) 0
                          $ CubicSolveAcc.valueAtX 10 0.001 (curveShadows :: Exp (CubicBezier Double)) x
          strHighlights x = A.cond (x A.<=* 0.5) 0
                          $ A.cond (x A.>=* 1) 1
                          $ CubicSolveAcc.valueAtX 10 0.001 (curveHighlights :: Exp (CubicBezier Double)) x

          correctMasterR = colorCorrect masterContrastR masterGammaR masterGainR masterOffsetR
          correctMasterG = colorCorrect masterContrastG masterGammaG masterGainG masterOffsetG
          correctMasterB = colorCorrect masterContrastB masterGammaB masterGainB masterOffsetB

          correctShadowsR = colorCorrect (shadowsContrastR-1) (shadowsGammaR-1) (shadowsGainR-1) shadowsOffsetR
          correctShadowsG = colorCorrect (shadowsContrastG-1) (shadowsGammaG-1) (shadowsGainG-1) shadowsOffsetG
          correctShadowsB = colorCorrect (shadowsContrastB-1) (shadowsGammaB-1) (shadowsGainB-1) shadowsOffsetB

          correctMidtonesR = colorCorrect (midtonesContrastR-1) (midtonesGammaR-1) (midtonesGainR-1) midtonesOffsetR
          correctMidtonesG = colorCorrect (midtonesContrastG-1) (midtonesGammaG-1) (midtonesGainG-1) midtonesOffsetG
          correctMidtonesB = colorCorrect (midtonesContrastB-1) (midtonesGammaB-1) (midtonesGainB-1) midtonesOffsetB

          correctHighlightsR = colorCorrect (highlightsContrastR-1) (highlightsGammaR-1) (highlightsGainR-1) highlightsOffsetR
          correctHighlightsG = colorCorrect (highlightsContrastG-1) (highlightsGammaG-1) (highlightsGainG-1) highlightsOffsetG
          correctHighlightsB = colorCorrect (highlightsContrastB-1) (highlightsGammaB-1) (highlightsGainB-1) highlightsOffsetB

          correct' master shadows midtones highlights x = correct'' shadows midtones highlights (master x)

          correct'' shadows midtones highlights x = let
                  coeffShadows    = strShadows x
                  coeffHighlights = strHighlights x
                  coeffMidtones   = 1 - coeffShadows - coeffHighlights
              in coeffShadows * shadows x + coeffMidtones * midtones x + coeffHighlights * highlights x

          rgb = unsafeGetRGB img

          rgbRsaturated = M.map (A.lift1 (saturateOnHSV' masterSaturationR shadowsSaturationR midtonesSaturationR highlightsSaturationR)) rgb
          rgbGsaturated = M.map (A.lift1 (saturateOnHSV' masterSaturationG shadowsSaturationG midtonesSaturationG highlightsSaturationG)) rgb
          rgbBsaturated = M.map (A.lift1 (saturateOnHSV' masterSaturationB shadowsSaturationB midtonesSaturationB highlightsSaturationB)) rgb

          saturateOnHSV' :: A.Exp Double -> A.Exp Double -> A.Exp Double -> A.Exp Double -> Color.RGB (A.Exp Double) -> Color.RGB (A.Exp Double)
          saturateOnHSV' masterSat shadowsSat midtonesSat highlightsSat pix =
              Color.toHSV pix & (\(Color.HSV h s v) ->
                  saturateOnHSV'' shadowsSat midtonesSat highlightsSat $ Color.toRGB $ Color.HSV h (s * (masterSat)) v)

          saturateOnHSV'' :: A.Exp Double -> A.Exp Double -> A.Exp Double -> Color.RGB (A.Exp Double) -> Color.RGB (A.Exp Double)
          saturateOnHSV'' shadowsSat midtonesSat highlightsSat pix =
              Color.toHSV pix & (\(Color.HSV h s v) -> let
                      coeffShadows    = strShadows v
                      coeffHighlights = strHighlights v
                      coeffMidtones = 1 - coeffShadows - coeffHighlights
                  in Color.HSV h (s * (coeffShadows * shadowsSat + coeffMidtones * midtonesSat + coeffHighlights * highlightsSat)) v) & Color.toRGB

          rSaturated = M.map (\(A.unlift -> Color.RGB r _ _) -> r) rgbRsaturated
          gSaturated = M.map (\(A.unlift -> Color.RGB _ g _) -> g) rgbGsaturated
          bSaturated = M.map (\(A.unlift -> Color.RGB _ _ b) -> b) rgbBsaturated

          Right view = lookupPrimary img

          view' = insertChannelFloats view [
                    ("rgba.r", rSaturated)
                  , ("rgba.g", gSaturated)
                  , ("rgba.b", bSaturated)
                  ]

          saturated = Image.singleton view' -- Image.update (const $ Just view') "rgba" img

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

          Right view = lookupPrimary img
          view' = insertChannelFloats view [
                      ("rgba.r", r')
                    , ("rgba.g", g')
                    , ("rgba.b", b')
                    , ("rgba.a", a')
                    ]
          img' = Image.insertPrimary view' img

liftF6 f t1 t2 t3 t4 t5 t6 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    t5' <- t5
    t6' <- t6
    val f <<*>> t1' <<*>> t2' <<*>> t3' <<*>> t4' <<*>> t5' <<*>> t6'

liftF21  f t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 = do
    t1'  <- t1
    t2'  <- t2
    t3'  <- t3
    t4'  <- t4
    t5'  <- t5
    t6'  <- t6
    t7'  <- t7
    t8'  <- t8
    t9'  <- t9
    t10' <- t10
    t11' <- t11
    t12' <- t12
    t13' <- t13
    t14' <- t14
    t15' <- t15
    t16' <- t16
    t17' <- t17
    t18' <- t18
    t19' <- t19
    t20' <- t20
    t21' <- t21
    val f <<*>> t1' <<*>> t2' <<*>> t3' <<*>> t4' <<*>> t5' <<*>> t6' <<*>> t7' <<*>> t8' <<*>> t9' <<*>> t10' <<*>> t11' <<*>> t12' <<*>> t13' <<*>> t14' <<*>> t15' <<*>> t16' <<*>> t17' <<*>> t18' <<*>> t19' <<*>> t20' <<*>> t21'

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
    where alphas = onShader (Stencil.stencil (+) (unsafeFromMatrix edgeOperator) (+) 0) img
          (r, g, b, _) = unsafeGetChannels alphas
          alphaSum = M.zipWith3 (\a b c -> a + b + c) r g b
          Right view = lookupPrimary img
          img' = Image.insertPrimary (insertChannelFloats view [("rgba.a", alphaSum)]) img

gammaToLinearLuna :: Gamma.Companding a (A.Exp Double) => a -> Image -> Image
gammaToLinearLuna companding = onEach $ (Gamma.toLinear companding :: A.Exp Double -> A.Exp Double)

gammaFromLinearLuna :: Gamma.Companding a (A.Exp Double) => a -> Image -> Image
gammaFromLinearLuna companding = onEach $ Gamma.fromLinear companding

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

toInterpolator :: (Elt e, IsFloating e) => InterpolationFilter (Exp e) -> DiscreteShader (Exp e) -> CartesianShader (Exp e) (Exp e)
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

-- TODO[KM]: ask someone what is this function supposed to do? now it might be obsolete to pattern match on MatrixData and perform M.map, doing this through Shaders would probably be a better idea
-- TODO^:    commented out for now(the boundary can't be explicitly typed here, we can have different data – why even try to interpolate all channels at the same time?)
--interpolateChannelsLuna :: A.Boundary Double -> InterpolationFilter Double -> Image -> Image
--interpolateChannelsLuna (fmap variable -> boundary) (toInterpolator . fmap variable -> interpol) = Image.map (View.map interpolate)
--    where interpolate (ChannelFloat name (asMatrixData -> MatrixData mat)) = ChannelFloat name $ ContinuousData $ toGen $ mat
--          interpolate (ChannelInt   name (asMatrixData -> MatrixData mat)) = ChannelInt   name $ ContinuousData $ toGen . M.map A.fromIntegral $ mat
--          interpolate (ChannelBit   name (asMatrixData -> MatrixData mat)) = ChannelBit   name $ ContinuousData $ toGen . M.map (A.fromIntegral . A.boolToInt) $ mat

--          toGen = interpol . fromMatrix boundary

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
    where multisample (asContinuous -> ChannelFloat name (ContinuousData gen)) = ChannelFloat name $ MatrixData . rasterizer . sampler $ gen
          --                                                            FIXME[MM]: ^ we don't want this here,
          --                                                                         but ChannelShader requires ContinuousShader :/
          --                                                            FIXME[KM]: ^ I've changed the structure of Channels so we might have to talk about this
          multisample channel                     = channel

-- FIXME[MM]: will remove the whole view if removing fails - it should somehow propagate the error
-- FIXME[KM][iup]: when fixing, we also have take into consideration the change to the Image.update function
--removeChannelLuna :: String -> String -> Image -> Image
--removeChannelLuna viewName channelName = Image.update f viewName
--    where f view = case View.remove channelName view of
--                  Left _ -> Nothing
--                  Right v -> Just v

getChannelLuna :: String -> String -> Image -> Image.Result (Maybe Channel)
getChannelLuna viewName channelName img = case Image.lookup viewName img of
    Right view -> View.get view channelName
    _         -> Left $ Image.ViewLookupError viewName

-- FIXME[KM]: [iup]
--insertChannelLuna :: String -> Channel -> Image -> Image
--insertChannelLuna viewName chan = Image.update f viewName
--    where f = Just . View.append chan

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

gradeLunaColor :: VPS (Color.RGBA Double)
               -> VPS (Color.RGBA Double)
               -> VPS (Color.RGBA Double)
               -> Color.RGBA Double
               -> Color.RGBA Double
               -> Color.RGBA Double
               -> Color.RGBA Double
               -> Image
               -> Image
gradeLunaColor (VPS (fmap variable -> Color.RGBA blackpointR blackpointG blackpointB blackpointA))
               (VPS (fmap variable -> Color.RGBA whitepointR whitepointG whitepointB whitepointA))
               (VPS (fmap variable -> Color.RGBA liftR liftG liftB liftA))
               (fmap variable -> Color.RGBA gainR gainG gainB gainA)
               (fmap variable -> Color.RGBA multiplyR multiplyG multiplyB multiplyA)
               (fmap variable -> Color.RGBA offsetR offsetG offsetB offsetA)
               (fmap variable -> Color.RGBA gammaR gammaG gammaB gammaA)
               = onEachRGBA (grade blackpointR whitepointR liftR gainR multiplyR offsetR gammaR)
                            (grade blackpointG whitepointG liftG gainG multiplyG offsetG gammaG)
                            (grade blackpointB whitepointB liftB gainB multiplyB offsetB gammaB)
                            id -- (grade blackpointA whitepointA liftA gainA multiplyA offsetA gammaA)

readFromEXRLuna :: FilePath -> IO Image
readFromEXRLuna path = fmap fromJust $ readFromEXR path

extension :: FilePath -> (FilePath, String)
extension path = (path, P.map toLower $ FilePath.takeExtension path)

pattern ImageEXR path <- (extension -> (path, ".exr"))

realReadLuna :: FilePath -> IO Image
realReadLuna (ImageEXR path) = readFromEXRLuna path
realReadLuna path            = loadImageLuna path

testColorCC :: Color5 -> Image
testColorCC (VPS (ColorD r _ _ _), VPS (ColorD _ g _ _), VPS (ColorD _ _ b _), VPS (ColorD _ _ _ a), VPS (ColorD _ _ _ x)) =
    constantLuna 512 512 $ Color.RGBA (r*x) (g*x) (b*x) (a*x)

