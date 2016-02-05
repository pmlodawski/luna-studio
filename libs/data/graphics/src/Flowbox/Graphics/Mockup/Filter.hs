---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}

module Flowbox.Graphics.Mockup.Filter (
    blurLuna,
    closeLuna,
    dilateLuna,
    ditherLuna,
    EdgeOperator (..),
    Orientation (..),
    EdgeBlur.BlurType (..),
    edgeDetectLuna,
    erodeLuna,
    histEqLuna,
    openLuna,
    posterizeLuna,
    edgeBlurLuna,
    laplacianOperator,
    scharrOperator,
    sobelOperator,
    prewittOperator
) where

import qualified Data.Array.Accelerate                  as A
import           Math.Space.Space                       (Grid (..))

import qualified Flowbox.Graphics.Color.Color           as Color
import qualified Flowbox.Graphics.Composition.Color     as CC
import           Flowbox.Graphics.Composition.Dither    (DiffusionTable)
import qualified Flowbox.Graphics.Composition.Dither    as Composition
import qualified Flowbox.Graphics.Composition.EdgeBlur  as EdgeBlur
import qualified Flowbox.Graphics.Composition.Filter    as Filter
import qualified Flowbox.Graphics.Composition.Histogram as Histogram
import           Flowbox.Graphics.Image.Channel         (Channel (..), ChannelData (..))
import qualified Flowbox.Graphics.Image.Channel         as Channel
import           Flowbox.Graphics.Image.Image           (Image)
import qualified Flowbox.Graphics.Image.Image           as Image
import qualified Flowbox.Graphics.Shader.Matrix         as Shader
import qualified Flowbox.Graphics.Shader.Pipe           as Shader
import           Flowbox.Graphics.Shader.Shader         (DiscreteShader)
import qualified Flowbox.Graphics.Shader.Stencil        as Stencil
import           Flowbox.Graphics.Utils.Accelerate      (variable)
import           Flowbox.Math.Matrix                    (Matrix2)
import           Flowbox.Math.Matrix                    as M
import           Flowbox.Prelude                        as P hiding (lookup)

import           Flowbox.Graphics.Mockup.Basic



--defocus :: Int -> Image -> Image
--defocus size = onEachChannel process
--    where kernel = ellipse (pure $ variable size) 1 (0 :: A.A.Exp Double)
--          process = rasterizer . normStencil (+) kernel (+) 0 . fromMatrix A.Clamp

--motionBlur :: Int -> Double -> Image -> Image
--motionBlur size angle = onEachChannel process
--    where kernel = monosampler
--                 $ rotateCenter (variable angle)
--                 $ Shader.nearest
--                 $ rectangle (Grid (variable size) 1) 1 0
--          process = rasterizer . normStencil (+) kernel (+) 0 . fromMatrix A.Clamp


edgeBlurLuna :: Channel.Name -> EdgeBlur.BlurType -> Int -> Float -> Image -> Image
edgeBlurLuna channelName blurType kernelSize edgeMultiplier image =
    case Image.getFromPrimary channelName image of
        Left err             -> error $ show err
        Right (Nothing)      -> image
        Right (Just channel) -> onEachChannel blurFunc image where
            blurFunc = \case
                (Channel.asDiscreteClamp -> ChannelFloat name (DiscreteData shader)) -> ChannelFloat name $ DiscreteData $ blurShader shader
                (Channel.asDiscreteClamp -> ChannelInt name (DiscreteData shader)) -> ChannelInt name $ DiscreteData $ mapShaderInt blurShader shader
            mapShaderInt func x = fmap (floor . (*256)) $ (( func $ fmap ((/256) . A.fromIntegral) x ) :: DiscreteShader (A.Exp Float))
            blurShader = EdgeBlur.maskBlur blurType (variable kernelSize) maskEdges
            maskEdges  = case channel of
                (Channel.asDiscreteClamp -> ChannelFloat name (DiscreteData shader)) -> EdgeBlur.edges (variable edgeMultiplier) shader
                (Channel.asDiscreteClamp -> ChannelInt name (DiscreteData shader)) -> EdgeBlur.edges (variable edgeMultiplier) $ fmap ((/256) . A.fromIntegral) shader

--bilateral :: Double
--          -> Double
--          -> Int
--          -> Image
--          -> Image
--bilateral psigma csigma (variable -> size) = onEachChannel process
--    where p = Shader.pipe A.Clamp
--          spatial :: Shader (Point2 (A.Exp Int)) (A.Exp Double)
--          spatial = Shader (pure $ variable size) $ \(Point2 x y) ->
--              let dst = sqrt . A.fromIntegral $ (x - size `div` 2) * (x - size `div` 2) + (y - size `div` 2) * (y - size `div` 2)
--              in apply (gauss $ variable psigma) dst
--          domain center neighbour = apply (gauss $ variable csigma) (abs $ neighbour - center)
--          process = rasterizer . (id `p` bilateralStencil (+) spatial domain (+) 0 `p` id) . fromMatrix A.Clamp

posterizeLuna :: Float -> Image -> Image
posterizeLuna (variable -> colors) = onEach $ CC.posterize colors

blurLuna :: Int -> Image -> Image
blurLuna (variable -> kernelSize) = onEachChannel blurChannel
    where blurChannel = \case
              (Channel.asDiscreteClamp -> ChannelFloat name zeData) -> ChannelFloat name $ (\(DiscreteData shader) -> DiscreteData $ processFloat shader) zeData
              (Channel.asDiscreteClamp -> ChannelInt   name zeData) -> ChannelInt   name $ (\(DiscreteData shader) -> DiscreteData $ processInt   shader) zeData
          processFloat x = id `p` Filter.filter 1 vmat `p` Filter.filter 1 hmat `p` id $ x
          processInt   x = fmap floor $ (processFloat $ fmap A.fromIntegral x :: DiscreteShader (A.Exp Float))
          p = Shader.pipe A.Clamp
          hmat :: (A.Elt e, A.IsFloating e) => Matrix2 e
          hmat = id M.>-> Filter.normalize $ Filter.toMatrix (Grid 1 kernelSize) $ Filter.gauss 1.0
          vmat :: (A.Elt e, A.IsFloating e) => Matrix2 e
          vmat = id M.>-> Filter.normalize $ Filter.toMatrix (Grid kernelSize 1) $ Filter.gauss 1.0

--laplacianLuna :: Int -> Double -> Double -> Image -> Image
--laplacianLuna (variable -> kernSize) (variable -> crossVal) (variable -> sideVal) img = img'
--    where img' = onEachChannel process img
--          process x = rasterizer $ id `p` Filter.filter 1 flt `p` id $ fromMatrix A.Clamp x
--          flt = laplacian crossVal sideVal $ pure kernSize
--          p = Shader.pipe A.Clamp

erodeLuna :: Int -> Image -> Image
erodeLuna (variable -> size) = onShader $ Filter.erode $ pure size

dilateLuna :: Int -> Image -> Image
dilateLuna (variable -> size) = onShader $ Filter.dilate $ pure size

closeLuna :: Int -> Image -> Image
closeLuna (variable -> size) = onShader $ Filter.closing $ pure size

openLuna :: Int -> Image -> Image
openLuna (variable -> size) = onShader $ Filter.opening $ pure size

histEqLuna :: Int -> Image -> Image
histEqLuna (variable -> bins) img = img'
    where rgb = unsafeGetRGB img
          hsv = M.map Color.liftedConvertColor rgb
          v   = M.map (\(A.unlift -> Color.HSV _ _ v) -> v) hsv
          v'  = M.Delayed $ Histogram.histeq bins (M.accMatrix v)
          hsv' = M.zipWith (\(A.unlift -> Color.HSV h s _) v -> A.lift $ Color.HSV h s v) hsv v'
          rgb' = M.map Color.liftedConvertColor hsv'
          (r, g, b) = M.unzip3 $ M.map (\(A.unlift -> Color.RGB r g b) -> A.lift (r, g, b)) rgb'

          Right view = Image.lookupPrimary img

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
        ditherMethod = Composition.dither boundary table bits
    r' <- mutableProcess temporaryBackend ditherMethod r
    g' <- mutableProcess temporaryBackend ditherMethod g
    b' <- mutableProcess temporaryBackend ditherMethod b

    let Right view = Image.lookupPrimary img
        view' = insertChannelFloats view [
                      ("rgba.r", r')
                    , ("rgba.g", g')
                    , ("rgba.b", b')
                  ]
        img' = Image.insertPrimary view' img

    return img'

--orderedDitherLuna :: Int -> Image -> Image
--orderedDitherLuna bits = onEachChannel $ bayer bits

data EdgeOperator = Prewitt Orientation
                  | Sobel Orientation
                  | Scharr Orientation
                  | Laplace

data Orientation = Vertical
                 | Horizontal

edgeDetectLuna :: EdgeOperator -> Image -> Image
edgeDetectLuna edgeOperator img =
    edgeDetectLuna' edgeOperatorMatrix img where
        edgeOperatorMatrix = case edgeOperator of
            Prewitt Vertical       -> M.transpose baseEdgeOperatorMatrix
            Sobel Vertical         -> M.transpose baseEdgeOperatorMatrix
            Scharr Vertical        -> M.transpose baseEdgeOperatorMatrix
            _                      -> baseEdgeOperatorMatrix
        baseEdgeOperatorMatrix = case edgeOperator of
            Prewitt _ -> Filter.prewitt
            Sobel _   -> Filter.sobel
            Scharr _  -> Filter.scharr
            Laplace   -> Filter.laplacian 1 1 (Grid 1 1)

edgeDetectLuna' :: Matrix2 Float -> Image -> Image
edgeDetectLuna' edgeOperator img = img'
    where alphas = onShader (Stencil.stencil (+) (Shader.unsafeFromMatrix edgeOperator) (+) 0) img
          (r, g, b, _) = unsafeGetChannels alphas
          alphaSum = M.zipWith3 (\a b c -> a + b + c) r g b
          Right view = Image.lookupPrimary img
          img' = Image.insertPrimary (insertChannelFloats view [("rgba.a", alphaSum)]) img

laplacianOperator :: Int -> Float -> Float -> Matrix2 Float
laplacianOperator (variable -> kernSize) (variable -> crossVal) (variable -> sideVal) = Filter.laplacian crossVal sideVal (pure kernSize)

sobelOperator :: Matrix2 Float
sobelOperator = Filter.sobel

prewittOperator :: Matrix2 Float
prewittOperator = Filter.prewitt

scharrOperator :: Matrix2 Float
scharrOperator = Filter.scharr

medianLuna :: Int -> Image -> Image
medianLuna size img = undefined

--radialBlurLuna :: Int -> Double -> Image -> Image
--radialBlurLuna (variable -> size) (variable -> angle) = onEachChannel process
--    where kern = monosampler
--               $ rotateCenter angle
--               $ Shader.nearest
--               $ rectangle (Grid size 1) 1 0
--          process = rasterizer
--                  . monosampler
--                  . foo
--          foo :: Matrix2 Double -> ContinuousShader (A.Exp Double)
--          foo     = translate (V2 (256) (256))
--                  . fromPolarMapping
--                  . Shader.nearest
--                  . normStencil (+) kern (+) 0
--                  . monosampler
--                  . (toPolarMapping :: ContinuousShader (A.Exp Double) -> ContinuousShader (A.Exp Double))
--                  . translate (V2 (-256) (-256))
--                  . Shader.nearest
--                  . fromMatrix A.Clamp
