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
import           Flowbox.Graphics.Image.Error                         as Image
import           Flowbox.Graphics.Composition.Merge                   (AlphaBlend(..))
import qualified Flowbox.Graphics.Composition.Merge                   as Merge
import           Flowbox.Graphics.Image.View                          as View
import           Flowbox.Graphics.Utils.Accelerate                    (variable)
import           Flowbox.Graphics.Utils.Utils
import           Flowbox.Math.Matrix                                  as M
import           Flowbox.Prelude                                      as P hiding (lookup)
import qualified Data.Array.Accelerate.CUDA                           as CUDA

import Luna.Target.HS (Pure (..), Safe (..), Value (..), autoLift, autoLift1, fromValue, val)
import Control.PolyApplicative ((<<*>>))

import Flowbox.Graphics.Mockup.Basic
import Flowbox.Graphics.Mockup.ColorCorrect
import Flowbox.Graphics.Mockup.Matte


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

testColorCC :: Color5 -> Image
testColorCC (VPS (ColorD r _ _ _), VPS (ColorD _ g _ _), VPS (ColorD _ _ b _), VPS (ColorD _ _ _ a), VPS (ColorD _ _ _ x)) =
    constantLuna 512 512 $ Color.RGBA (r*x) (g*x) (b*x) (a*x)

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

edgeDetectLuna :: Matrix2 Float -> Image -> Image
edgeDetectLuna edgeOperator img = img'
    where alphas = onShader (Stencil.stencil (+) (unsafeFromMatrix edgeOperator) (+) 0) img
          (r, g, b, _) = unsafeGetChannels alphas
          alphaSum = M.zipWith3 (\a b c -> a + b + c) r g b
          Right view = lookupPrimary img
          img' = Image.insertPrimary (insertChannelFloats view [("rgba.a", alphaSum)]) img

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
