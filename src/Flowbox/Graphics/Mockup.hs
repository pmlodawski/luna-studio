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
    , V2(..)
) where

import Flowbox.Graphics.Mockup.Basic
import Flowbox.Graphics.Mockup.Matte

import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Type        as A
import           Data.Array.Accelerate.CUDA
import           Data.Bool
import           Math.Coordinate.Cartesian
import           Math.Space.Space
import           Math.Metric
import           Linear                            (V2(..))

import qualified Flowbox.Graphics.Color.Color                         as Color
import qualified Flowbox.Graphics.Color.Companding                    as Gamma
import           Flowbox.Graphics.Composition.Dither
import           Flowbox.Geom2D.Accelerate.CubicBezier
import           Flowbox.Geom2D.Accelerate.CubicBezier.Solve          as CubicSolveAcc
import           Flowbox.Geom2D.CubicBezier
import           Flowbox.Geom2D.Rectangle
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
import           Flowbox.Graphics.Shader.Sampler                      as Shader
import           Flowbox.Graphics.Composition.Generator.Shape
import           Flowbox.Graphics.Shader.Stencil                      as Stencil
import           Flowbox.Graphics.Shader.Shader                       as Shader
import           Flowbox.Graphics.Composition.Transform               as Transform
import           Flowbox.Graphics.Composition.Histogram
import qualified Flowbox.Graphics.Composition.Generator.Raster        as Raster
import           Flowbox.Graphics.Image.Channel                       as Channel
import           Flowbox.Graphics.Composition.Color
import           Flowbox.Graphics.Image.Image                         as Image
import qualified Flowbox.Graphics.Image.Matte                         as Matte
import           Flowbox.Graphics.Image.Error                         as Image
import           Flowbox.Graphics.Composition.Merge                   (AlphaBlend(..))
import qualified Flowbox.Graphics.Composition.Merge                   as Merge
import           Flowbox.Graphics.Image.View                          as View
import           Flowbox.Graphics.Utils.Accelerate                    (variable)
import           Flowbox.Graphics.Utils.Utils
import           Flowbox.Math.Matrix                                  as M
import           Flowbox.Prelude                                      as P hiding (lookup)
import qualified Data.Array.Accelerate.CUDA                           as CUDA
import           Flowbox.Math.Function.Accelerate.BSpline             as BSpline
import qualified Flowbox.Math.Function.CurveGUI                       as CurveGUI

import Luna.Target.HS (Pure (..), Safe (..), Value (..), autoLift, autoLift1, fromValue, val)
import Control.PolyApplicative ((<<*>>))



type ColorD = Color.RGBA Float
pattern ColorD r g b a = Color.RGBA r g b a
type Color5 = (VPS ColorD, VPS ColorD, VPS ColorD, VPS ColorD, VPS ColorD)

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


edgeBlur :: Channel.Name -> EB.BlurType -> Int -> Float -> Image -> Image
edgeBlur channelName blurType kernelSize edgeMultiplier image =
    case getFromPrimary channelName image of
        Left err             -> error $ show err
        Right (Nothing)      -> image
        Right (Just channel) -> onEachChannel blurFunc image where
            blurFunc = \case
                (Channel.asDiscreteClamp -> ChannelFloat name (DiscreteData shader)) -> ChannelFloat name $ DiscreteData $ blurShader shader
                (Channel.asDiscreteClamp -> ChannelInt name (DiscreteData shader)) -> ChannelInt name $ DiscreteData $ mapShaderInt blurShader shader
            mapShaderInt func x = fmap (floor . (*256)) $ (( func $ fmap ((/256) . A.fromIntegral) x ) :: DiscreteShader (Exp Float))
            blurShader = EB.maskBlur blurType (variable kernelSize) maskEdges
            maskEdges  = case channel of
                (Channel.asDiscreteClamp -> ChannelFloat name (DiscreteData shader)) -> EB.edges (variable edgeMultiplier) shader
                (Channel.asDiscreteClamp -> ChannelInt name (DiscreteData shader)) -> EB.edges (variable edgeMultiplier) $ fmap ((/256) . A.fromIntegral) shader


-- rotateCenter :: (Elt a, IsFloating a) => Exp a -> CartesianShader (Exp a) b -> CartesianShader (Exp a) b
--rotateCenter phi = canvasT (fmap A.ceiling . rotate phi . asFloating) . onCenter (rotate phi)


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

offsetMatteLuna :: Color.RGBA Float -> Maybe (Matte.Matte Float) -> Image -> Image
offsetMatteLuna x@(fmap variable -> Color.RGBA r g b a) matte img =
  case matte of
    Nothing -> onEachRGBA (offset r) (offset g) (offset b) (offset a) img
    Just m ->
        onEachRGBAChannels (applyMatteFloat (offset r) m)
                           (applyMatteFloat (offset g) m)
                           (applyMatteFloat (offset b) m)
                           (applyMatteFloat (offset a) m) img

contrastMatteLuna :: Color.RGBA Float -> Maybe (Matte.Matte Float) -> Image -> Image
contrastMatteLuna x@(fmap variable -> Color.RGBA r g b a) matte img =
  case matte of
    Nothing -> onEachRGBA (contrast r) (contrast g) (contrast b) (contrast a) img
    Just m ->
        onEachRGBAChannels (applyMatteFloat (contrast r) m)
                           (applyMatteFloat (contrast g) m)
                           (applyMatteFloat (contrast b) m)
                           (applyMatteFloat (contrast a) m) img

exposureMatteLuna :: Color.RGBA Float -> Color.RGBA Float -> Maybe (Matte.Matte Float) -> Image -> Image
exposureMatteLuna x@(fmap variable -> Color.RGBA blackpointR blackpointG blackpointB blackpointA)
                  y@(fmap variable -> Color.RGBA exR exG exB exA) matte img =
                    case matte of
                      Nothing -> onEachRGBA (exposure blackpointR exR) (exposure blackpointG exG) (exposure blackpointB exB) id img -- (exposure blackpointA exA)
                      Just m ->
                          onEachRGBAChannels (applyMatteFloat (exposure blackpointR exR) m)
                                             (applyMatteFloat (exposure blackpointG exG) m)
                                             (applyMatteFloat (exposure blackpointB exB) m)
                                             (applyMatteFloat (exposure blackpointA exA) m) img

gradeLunaColorMatte :: VPS (Color.RGBA Float)
                    -> VPS (Color.RGBA Float)
                    -> VPS (Color.RGBA Float)
                    -> VPS (Color.RGBA Float)
                    -> Color.RGBA Float
                    -> Color.RGBA Float
                    -> Color.RGBA Float
                    -> Maybe (Matte.Matte Float)
                    -> Image
                    -> Image
gradeLunaColorMatte (VPS (fmap variable -> Color.RGBA blackpointR blackpointG blackpointB blackpointA))
                    (VPS (fmap variable -> Color.RGBA whitepointR whitepointG whitepointB whitepointA))
                    (VPS (fmap variable -> Color.RGBA liftR liftG liftB liftA))
                    (VPS (fmap variable -> Color.RGBA gainR gainG gainB gainA))
                    (fmap variable -> Color.RGBA multiplyR multiplyG multiplyB multiplyA)
                    (fmap variable -> Color.RGBA offsetR offsetG offsetB offsetA)
                    (fmap variable -> Color.RGBA gammaR gammaG gammaB gammaA) matte img =
                      case matte of
                        Nothing -> onEachRGBA (grade blackpointR whitepointR liftR gainR multiplyR offsetR gammaR)
                                              (grade blackpointG whitepointG liftG gainG multiplyG offsetG gammaG)
                                              (grade blackpointB whitepointB liftB gainB multiplyB offsetB gammaB)
                                              id img -- (grade blackpointA whitepointA liftA gainA multiplyA offsetA gammaA)
                        Just m -> onEachRGBAChannels (applyMatteFloat (grade blackpointR whitepointR liftR gainR multiplyR offsetR gammaR) m)
                                                     (applyMatteFloat (grade blackpointG whitepointG liftG gainG multiplyG offsetG gammaG) m)
                                                     (applyMatteFloat (grade blackpointB whitepointB liftB gainB multiplyB offsetB gammaB) m)
                                                     (applyMatteFloat (grade blackpointA whitepointA liftA gainA multiplyA offsetA gammaA) m) img

offsetLuna :: Color.RGBA Float -> Image -> Image
offsetLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (offset r) (offset g) (offset b) id -- (offset a)

contrastLuna :: Color.RGBA Float -> Image -> Image
contrastLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (contrast r) (contrast g) (contrast b) id -- (contrast a)

exposureLuna :: Color.RGBA Float -> Color.RGBA Float -> Image -> Image
exposureLuna (fmap variable -> Color.RGBA blackpointR blackpointG blackpointB blackpointA)
             (fmap variable -> Color.RGBA exR exG exB exA) =
                 onEachRGBA (exposure blackpointR exR)
                            (exposure blackpointG exG)
                            (exposure blackpointB exB)
                            id -- (exposure blackpointA exA)

gradeLuna :: VPS Float -> VPS Float -> VPS Float -> Float -> Float -> Float -> Float -> Image -> Image
gradeLuna (VPS (variable -> blackpoint))
          (VPS (variable -> whitepoint))
          (VPS (variable -> lift))
          (variable -> gain)
          (variable -> multiply')
          (variable -> offset')
          (variable -> gamma') =
            onEach $ grade blackpoint whitepoint lift gain multiply' offset' gamma'

saturateLuna :: Color.RGBA Float -> Image -> Image
saturateLuna (fmap variable -> Color.RGBA saturationR saturationG saturationB saturationA) img = saturated
    where rgb = unsafeGetRGB img

          rgbRsaturated = M.map (A.lift1 (saturateOnHSV saturationR)) rgb
          rgbGsaturated = M.map (A.lift1 (saturateOnHSV saturationG)) rgb
          rgbBsaturated = M.map (A.lift1 (saturateOnHSV saturationB)) rgb

          saturateOnHSV :: A.Exp Float -> Color.RGB (A.Exp Float) -> Color.RGB (A.Exp Float)
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

posterizeLuna :: Float -> Image -> Image
posterizeLuna (variable -> colors) = onEach $ posterize colors

keyer' :: (A.Exp (Color.RGB Float) -> A.Exp Float) -> Image -> Image
keyer' f img = img'
    where rgb = unsafeGetRGB img
          Right view = lookupPrimary img
          alpha = M.map f rgb

          view' = insertChannelFloats view [("rgba.a", alpha)]

          img' = Image.insertPrimary view' img

keyerLuna :: KeyerMode -> Float -> Float -> Float -> Float -> Image -> Image
keyerLuna mode (variable -> a) (variable -> b) (variable -> c) (variable -> d) img =
    keyer' (keyer mode (A.lift (a, b, c, d))) img

differenceKeyer' :: (A.Exp (Color.RGB Float) -> A.Exp (Color.RGB Float) -> A.Exp Float) -> Image -> Image -> Image
differenceKeyer' f background foreground = img'
    where backgroundRGB = unsafeGetRGB background
          foregroundRGB = unsafeGetRGB foreground

          alpha = M.map (A.uncurry f) $ M.zip backgroundRGB foregroundRGB

          Right view = lookupPrimary foreground
          view' = insertChannelFloats view [("rgba.a", alpha)]

          img' = Image.insertPrimary view' foreground

differenceKeyerLuna :: Float -> Float -> Image -> Image -> Image
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

blurLuna :: Int -> Image -> Image
blurLuna (variable -> kernelSize) = onEachChannel blurChannel
    where blurChannel = \case
              (Channel.asDiscreteClamp -> ChannelFloat name zeData) -> ChannelFloat name $ (\(DiscreteData shader) -> DiscreteData $ processFloat shader) zeData
              (Channel.asDiscreteClamp -> ChannelInt   name zeData) -> ChannelInt   name $ (\(DiscreteData shader) -> DiscreteData $ processInt   shader) zeData
          processFloat x = id `p` Conv.filter 1 vmat `p` Conv.filter 1 hmat `p` id $ x
          processInt   x = fmap floor $ (processFloat $ fmap A.fromIntegral x :: DiscreteShader (Exp Float))
          p = pipe A.Clamp
          hmat :: (Elt e, IsFloating e) => Matrix2 e
          hmat = id M.>-> normalize $ toMatrix (Grid 1 kernelSize) $ gauss 1.0
          vmat :: (Elt e, IsFloating e) => Matrix2 e
          vmat = id M.>-> normalize $ toMatrix (Grid kernelSize 1) $ gauss 1.0

--laplacianLuna :: Int -> Double -> Double -> Image -> Image
--laplacianLuna (variable -> kernSize) (variable -> crossVal) (variable -> sideVal) img = img'
--    where img' = onEachChannel process img
--          process x = rasterizer $ id `p` Conv.filter 1 flt `p` id $ fromMatrix A.Clamp x
--          flt = laplacian crossVal sideVal $ pure kernSize
--          p = pipe A.Clamp

constantLuna :: Int -> Int -> Color.RGBA Float -> Image
constantLuna (variable -> width) (variable -> height) (fmap variable -> Color.RGBA r g b a) =
    Raster.constant (A.index2 height width) chans
    where chans = [ ("rgba.r", r)
                  , ("rgba.g", g)
                  , ("rgba.b", b)
                  , ("rgba.a", a)
                  ]

--TODO[KM]: port checkerboard to luna
--type CheckerboardColorsLuna = (VPS ColorD, VPS ColorD, VPS ColorD, VPS ColorD)
--type CheckerboardLineLuna   = (VPS ColorD, VPS Double)
-- ...
--checkerboardLuna :: VPS Int -> VPS Int -> Double -> CheckerboardColorsLuna -> CheckerboardLineLuna -> CheckerboardLineLuna -> Image
--checkerboardLuna w h

circularLuna :: Int -> Int -> Image
circularLuna = gradientLuna circularShape

conicalLuna :: Int -> Int -> Image
conicalLuna = gradientLuna conicalShape

squareLuna :: Int -> Image
squareLuna side = gradientLuna squareShape side side

diamondLuna :: Int -> Int -> Image
diamondLuna = gradientLuna diamondShape

radialShapeLuna :: (Metric a (Point2 (Exp Float)) (Exp Float), MetricCoord a Cartesian)
                => a -> Int -> Int -> Image
radialShapeLuna metric w h = gradientLuna (radialShape metric) w h

linearShapeLuna :: Int -> Int -> Image
linearShapeLuna = gradientLuna linearShape

gradientLuna :: forall e.
                      (A.Lift Exp e,
                       A.Plain e ~ Int) =>
                      Shader (Point2 (Exp Float)) (Exp Float) -> e -> e -> Image
gradientLuna gradient (variable -> width) (variable -> height) = channelToImageRGBA grad
    where grad = rasterizer $ monosampler $ gradientShader

          gradientShader = scale (Grid width height) $ Transform.translate (V2 0.5 0.5) $ mapper gray gradient
          gray   = [Tick 0.0 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]

          weightFun tickPos val1 weight1 val2 weight2 = mix tickPos val1 val2
          mapper = flip colorMapper weightFun

perlinLuna :: Float -> Int -> Int -> Image
perlinLuna (variable -> z) = noiseLuna (perlinNoise z)

billowLuna :: Float -> Int -> Int -> Image
billowLuna (variable -> z) = noiseLuna (billowNoise z)

noiseLuna :: forall e a.
                   (IsFloating a, Elt a, A.Lift Exp e,
                    A.Plain e ~ Int) =>
                   CartesianShader (Exp a) (Exp Float) -> e -> e -> Image
noiseLuna noise (variable -> width) (variable -> height) = channelToImageRGBA noise'
    where noise' = rasterizer $ monosampler $ noiseShader

          noiseShader = scale (Grid width height) noise

cropLuna :: Rectangle Int -> Image -> Image
cropLuna rect = onEachChannel cropChannel
    where cropChannel = \case
              ChannelFloat name zeData -> ChannelFloat name $ Transform.crop rect zeData
              ChannelInt   name zeData -> ChannelInt   name $ Transform.crop rect zeData

hsvToolLuna :: VPS Float -> VPS Float -> VPS Float -> VPS Float
            -> VPS Float -> VPS Float -> VPS Float -> VPS Float
            -> VPS Float -> VPS Float -> VPS Float -> VPS Float
            -> A.Exp (Color.RGB Float)
            -> A.Exp (Color.RGB Float)
hsvToolLuna (VPS (variable -> hueRangeStart)) (VPS (variable -> hueRangeEnd))
            (VPS (variable -> hueRotation)) (VPS (variable -> hueRolloff))
            (VPS (variable -> saturationRangeStart)) (VPS (variable -> saturationRangeEnd))
            (VPS (variable -> saturationAdjustment)) (VPS (variable -> saturationRolloff))
            (VPS (variable -> brightnessRangeStart)) (VPS (variable -> brightnessRangeEnd))
            (VPS (variable -> brightnessAdjustment)) (VPS (variable -> brightnessRolloff)) =
    A.lift1 (hsvTool (A.lift $ Range hueRangeStart hueRangeEnd) hueRotation hueRolloff
                     (A.lift $ Range saturationRangeStart saturationRangeEnd) saturationAdjustment saturationRolloff
                     (A.lift $ Range brightnessRangeStart brightnessRangeEnd) brightnessAdjustment brightnessRolloff :: Color.RGB (A.Exp Float) -> Color.RGB (A.Exp Float))

hsvToolLuna' :: Float -> Float -> Float -> Float
             -> Float -> Float -> Float -> Float
             -> Float -> Float -> Float -> Float
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
                     (A.lift $ Range brightnessRangeStart brightnessRangeEnd) brightnessAdjustment brightnessRolloff :: Color.RGB (A.Exp Float) -> Color.RGB (A.Exp Float))

-- hsvToolLuna'' :: VPS Double -> VPS Double -> VPS Double -> VPS Double
--      -> VPS Double -> VPS Double -> VPS Double -> VPS Double
--      -> VPS Double -> VPS Double -> VPS Double -> VPS Double
--      -> VPS (Image) -> VPS (Image)
hsvToolLuna'' = liftF13 hsvToolLuna'

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

withAlpha :: (A.Exp Float -> A.Exp Float -> A.Exp Float) -> Image -> Image
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

colorMatrixLuna :: ColorMatrix Color.RGB Float -> Image -> Image
colorMatrixLuna matrix = onEachColorRGB (A.lift1 $ (colorMatrix :: ColorMatrix Color.RGB Float -> Color.RGB (A.Exp Float) -> Color.RGB (A.Exp Float)) matrix)

clampLuna :: (VPS Float, VPS Float) -> Maybe (VPS Float, VPS Float) -> Image -> Image
clampLuna (VPS (variable -> thLo), VPS (variable -> thHi)) clamps =
    case clamps of
        Just (VPS clampLo, VPS clampHi) -> onEach $ clamp (Range thLo thHi) $ Just $ Range (variable clampLo) (variable clampHi)
        _                               -> onEach $ clamp (Range thLo thHi) Nothing

multiplyLuna :: Color.RGBA Float -> Image -> Image
multiplyLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (*r) (*g) (*b) id -- (*a)

gammaLuna :: Color.RGBA Float -> Image -> Image
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

ditherLuna :: A.Boundary Float -> Int -> DiffusionTable Float -> Image -> IO Image
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

type LunaHandleGUI = (VPS Int, VPS Float, VPS Float)
type LunaControlPointGUI a = (VPS (Point2 a), VPS LunaHandleGUI, VPS LunaHandleGUI)
type LunaCurveGUI a = [VPS (LunaControlPointGUI a)]

convertHandleGUI :: LunaHandleGUI -> CurveGUI.Handle
convertHandleGUI (unpackLunaVar -> t, unpackLunaVar -> w, unpackLunaVar -> a) =
    case t of
        0 -> CurveGUI.NonLinear w a
        1 -> CurveGUI.Vertical w
        2 -> CurveGUI.Linear

getValueAtCurveGUI :: LunaCurveGUI Float -> Float -> Float
getValueAtCurveGUI (convertCurveGUI -> curve) = CurveGUI.valueAtSpline curve

convertControlPointGUI :: LunaControlPointGUI a -> CurveGUI.ControlPoint a
convertControlPointGUI (unpackLunaVar -> p, unpackLunaVar -> hIn, unpackLunaVar -> hOut) =
    CurveGUI.ControlPoint p (convertHandleGUI hIn) (convertHandleGUI hOut)

convertCurveGUI :: LunaCurveGUI a -> CurveGUI.Curve a
convertCurveGUI (unpackLunaList -> c) = CurveGUI.BezierCurve (fmap convertControlPointGUI c)

hueCorrectLuna :: VPS (LunaCurveGUI Float) -> VPS (LunaCurveGUI Float) ->
                  VPS (LunaCurveGUI Float) -> VPS (LunaCurveGUI Float) -> VPS (LunaCurveGUI Float) ->
                  LunaCurveGUI Float -> LunaCurveGUI Float -> LunaCurveGUI Float ->
                  -- GUICurve Float -> sat_thrsh will be added later
                  -- sat_thrsh affects only r,g,b and lum parameters
                  Image -> Image
hueCorrectLuna (VPS (convertCurveGUI-> lum)) (VPS (convertCurveGUI -> sat))
               (VPS (convertCurveGUI -> r)) (VPS (convertCurveGUI-> g))
               (VPS (convertCurveGUI -> b)) (convertCurveGUI -> rSup)
               (convertCurveGUI -> gSup) (convertCurveGUI-> bSup) img
                    = onEachColorRGB (hueCorrect (CurveGUI.convertToBSpline lum)
                                                 (CurveGUI.convertToBSpline sat)
                                                 (CurveGUI.convertToBSpline r)
                                                 (CurveGUI.convertToBSpline g)
                                                 (CurveGUI.convertToBSpline b)
                                                 (CurveGUI.convertToBSpline rSup)
                                                 (CurveGUI.convertToBSpline gSup)
                                                 (CurveGUI.convertToBSpline bSup)
                                     ) img

gradeLuna' :: VPS (Color.RGBA Float)
           -> VPS (Color.RGBA Float)
           -> VPS (Color.RGBA Float)
           -> Color.RGBA Float
           -> Color.RGBA Float
           -> Color.RGBA Float
           -> Color.RGBA Float
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

type ColorCorrect a = (VPS (LunaCurveGUI a), VPS (LunaCurveGUI a))
pattern ColorCorrect a b = (VPS a, VPS b)

colorCorrectLunaCurves :: VPS (ColorCorrect Float)
                       -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                       -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                       -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                       -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                       -> Image
                       -> Image
colorCorrectLunaCurves (VPS (ColorCorrect curveShadows curveHighlights)) = colorCorrectLunaBase (prepare curveShadows, prepare curveHighlights)
    where prepare (convertCurveGUI -> CurveGUI.BezierCurve nodes) = let nodes' = CurveGUI.convertToNodeList nodes in A.fromList (Z :. length nodes') nodes'

colorCorrectLuna' :: Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                  -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                  -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                  -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                  -> Image
                  -> Image
colorCorrectLuna' = colorCorrectLunaBase (curveShadows, curveHighlights)
    where curveShadows    = makeSpline [BSplineNode (Point2 0 1) (Point2 (-1) 1) (Point2 0.03 1), BSplineNode (Point2 0.09 0) (Point2 0.06 0) (Point2 1.09 0)]
          curveHighlights = makeSpline [BSplineNode (Point2 0.5 0) (Point2 (-0.5) 0) (Point2 (2/3) 0), BSplineNode (Point2 1 1) (Point2 (5/6) 1) (Point2 2 1)]
          makeSpline      = A.fromList (Z :. 2)

colorCorrectLunaBase :: (BSpline Float, BSpline Float)
                     -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                     -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                     -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                     -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                     -> Image
                     -> Image
colorCorrectLunaBase (curveShadows, curveHighlights)
                  ( VPS (fmap variable -> ColorD masterSaturationR masterSaturationG masterSaturationB masterSaturationA)
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
          strShadows x    = A.cond (x A.<=* 0) 1
                          $ A.cond (x A.>=* 0.09) 0
                          $ BSpline.valueAt (A.use curveShadows :: A.Acc (BSpline Float)) x
          strHighlights x = A.cond (x A.<=* 0.5) 0
                          $ A.cond (x A.>=* 1) 1
                          $ BSpline.valueAt (A.use curveHighlights :: A.Acc (BSpline Float)) x

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

          saturateOnHSV' :: A.Exp Float -> A.Exp Float -> A.Exp Float -> A.Exp Float -> Color.RGB (A.Exp Float) -> Color.RGB (A.Exp Float)
          saturateOnHSV' masterSat shadowsSat midtonesSat highlightsSat pix =
              Color.toHSV pix & (\(Color.HSV h s v) ->
                  saturateOnHSV'' shadowsSat midtonesSat highlightsSat $ Color.toRGB $ Color.HSV h (s * (masterSat)) v)

          saturateOnHSV'' :: A.Exp Float -> A.Exp Float -> A.Exp Float -> Color.RGB (A.Exp Float) -> Color.RGB (A.Exp Float)
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

edgeDetectLuna :: Matrix2 Float -> Image -> Image
edgeDetectLuna edgeOperator img = img'
    where alphas = onShader (Stencil.stencil (+) (unsafeFromMatrix edgeOperator) (+) 0) img
          (r, g, b, _) = unsafeGetChannels alphas
          alphaSum = M.zipWith3 (\a b c -> a + b + c) r g b
          Right view = lookupPrimary img
          img' = Image.insertPrimary (insertChannelFloats view [("rgba.a", alphaSum)]) img

gammaToLinearLuna :: Gamma.Companding a (A.Exp Float) => a -> Image -> Image
gammaToLinearLuna companding = onEach $ (Gamma.toLinear companding :: A.Exp Float -> A.Exp Float)

gammaFromLinearLuna :: Gamma.Companding a (A.Exp Float) => a -> Image -> Image
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

toMultisampler :: Grid (Exp Int) -> InterpolationFilter (Exp Float) -> Sampler Float
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

multisampleChannelsLuna :: Grid Int -> InterpolationFilter Float -> Image -> Image
multisampleChannelsLuna (fmap variable -> grid) (toMultisampler grid . fmap variable -> sampler :: Sampler Float) = Image.map (View.map multisample)
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

-- FIXME[KM]: [iup]
--insertChannelLuna :: String -> Channel -> Image -> Image
--insertChannelLuna viewName chan = Image.update f viewName
--    where f = Just . View.append chan

gradeLunaColor :: VPS (Color.RGBA Float)
               -> VPS (Color.RGBA Float)
               -> VPS (Color.RGBA Float)
               -> Color.RGBA Float
               -> Color.RGBA Float
               -> Color.RGBA Float
               -> Color.RGBA Float
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

testColorCC :: Color5 -> Image
testColorCC (VPS (ColorD r _ _ _), VPS (ColorD _ g _ _), VPS (ColorD _ _ b _), VPS (ColorD _ _ _ a), VPS (ColorD _ _ _ x)) =
    constantLuna 512 512 $ Color.RGBA (r*x) (g*x) (b*x) (a*x)
