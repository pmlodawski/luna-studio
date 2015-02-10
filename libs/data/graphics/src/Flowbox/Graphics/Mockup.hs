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

import qualified Codec.Picture.Png                 as Juicy
import qualified Codec.Picture.Types               as Juicy
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Array.Sugar as As
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
import qualified Flowbox.Graphics.Composition.Transform as Transform
import           Flowbox.Graphics.Composition.Histogram
import qualified Flowbox.Graphics.Composition.Generator.Raster                  as Raster
import           Flowbox.Graphics.Image.Channel                       as Channel
import           Flowbox.Graphics.Composition.Color
import           Flowbox.Graphics.Image.Image                         as Image
import qualified Flowbox.Graphics.Image.Matte                         as Matte
import           Flowbox.Graphics.Image.Error                         as Image
import           Flowbox.Graphics.Image.IO.ImageMagick                (loadImage, saveImage)
import           Flowbox.Graphics.Image.IO.OpenEXR                    (readFromEXR)
import           Flowbox.Graphics.Composition.Merge                         (AlphaBlend(..))
import qualified Flowbox.Graphics.Composition.Merge                   as Merge
import           Flowbox.Graphics.Image.View                          as View
import           Flowbox.Graphics.Utils.Utils
import           Flowbox.Math.Matrix                                  as M
import           Flowbox.Prelude                                      as P hiding (lookup)
import qualified Data.Array.Accelerate.CUDA                           as CUDA
import           Flowbox.Math.Function.Accelerate.BSpline             as BSpline
import qualified Flowbox.Math.Function.CurveGUI                       as CurveGUI

import Luna.Target.HS (Pure (..), Safe (..), Value (..), autoLift, autoLift1, fromValue, val)
import Control.PolyApplicative ((<<*>>))

-- something should be done with this
temporaryBackend :: M.Backend
temporaryBackend = CUDA.run

data SkewOrder = SkewXY | SkewYX

data Skew a = Skew { _skewValue :: V2 a
                   , _skewOrder :: SkewOrder
                   }

data Transform a = Transform { _translate :: V2 a
                             , _rotate    :: a
                             , _scale     :: V2 a
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
                         A.Vectors (As.EltRepr e)
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

onEachRGBAChannels :: (Channel -> Channel)
                   -> (Channel -> Channel)
                   -> (Channel -> Channel)
                   -> (Channel -> Channel)
                   -> Image
                   -> Image
onEachRGBAChannels fr fg fb fa img = img'
  where ChannelFloat _ (MatrixData r) = fr (getChan "rgba.r")
        ChannelFloat _ (MatrixData g) = fg (getChan "rgba.g")
        ChannelFloat _ (MatrixData b) = fb (getChan "rgba.b")
        ChannelFloat _ (MatrixData a) = fa (getChan "rgba.a")

        Right view = lookupPrimary img

        view' = insertChannelFloats view [
                    ("rgba.r", r)
                  , ("rgba.g", g)
                  , ("rgba.b", b)
                  , ("rgba.a", a)
                ]

        img' = Image.insertPrimary view' img
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


edgeBlur :: Channel.Name -> EB.BlurType -> Int -> Double -> Image -> Image
edgeBlur channelName blurType kernelSize edgeMultiplier image =
    case getFromPrimary channelName image of
        Left err             -> error $ show err
        Right (Nothing)      -> image
        Right (Just channel) -> onEachChannel blurFunc image where
            blurFunc = \case
                (Channel.asDiscreteClamp -> ChannelFloat name (DiscreteData shader)) -> ChannelFloat name $ DiscreteData $ blurShader shader
                (Channel.asDiscreteClamp -> ChannelInt name (DiscreteData shader)) -> ChannelInt name $ DiscreteData $ mapShaderInt blurShader shader
            mapShaderInt func x = fmap (floor . (*256)) $ (( func $ fmap ((/256) . A.fromIntegral) x ) :: DiscreteShader (Exp Double))
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

imageMatteLuna :: FilePath -> String -> IO (Maybe (Matte.Matte Double))
imageMatteLuna path channelName = do
  img <- realReadLuna path
  let channel = getChannelFromPrimaryLuna channelName img
  case channel of
    Right (Just channel) -> return $ Just $ Matte.imageMatteDouble channel
    _ -> return Nothing

vectorMatteLuna :: Mask2 Double -> Maybe (Matte.Matte Double)
vectorMatteLuna mask = Just $ Matte.VectorMatte $ convertMask mask

unpackAcc :: (A.Exp Int,A.Exp Int) -> (Int,Int)
unpackAcc (x,y) =
  let
    pair = A.lift (x,y) :: A.Exp (Int,Int)
    scalarPair = A.unit pair :: A.Acc (A.Scalar (Int,Int))
    l = temporaryBackend $ scalarPair :: A.Scalar (Int,Int)
    sh' = A.toList l :: [(Int,Int)]
    x' = fst $ head sh'
    y' = snd $ head sh'
  in
    (x',y')

adjustMatte :: Matrix2 Double -> Matrix2 Double -> Matrix2 Double
adjustMatte mat matte = matte'
  where
    sh = M.shape matte
    A.Z A.:. h A.:. w = A.unlift sh :: A.Z A.:. (A.Exp Int) A.:. (A.Exp Int)

    matte' = M.generate (M.shape mat) (\sh ->
      let
        A.Z A.:. x A.:. y = A.unlift sh :: A.Z A.:. (A.Exp Int) A.:. (A.Exp Int)
      in
        (x A.<* h A.&&* y A.<* w) A.? (matte M.! sh, 0.0))

applyMatteFloat :: (A.Exp Double -> A.Exp Double) -> Matte.Matte Double -> Channel -> Channel
applyMatteFloat f m (ChannelFloat name (MatrixData mat)) = ChannelFloat name (MatrixData mat')
  where
    sh = M.shape mat
    A.Z A.:. x A.:. y = A.unlift sh :: A.Z A.:. (A.Exp Int) A.:. (A.Exp Int)
    (h,w) = unpackAcc (x,y)
    matte = Matte.matteToMatrix h w m
    mat' = applyToMatrix f (adjustMatte mat matte) mat

applyMatteFloat f m (ChannelFloat name (DiscreteData shader)) = ChannelFloat name (DiscreteData shader')
  where
    Shader (Grid h' w') _ = shader
    (h,w) = unpackAcc (h',w')
    matte = Matte.matteToDiscrete h w m
    shader' = applyToShader f matte shader

applyMatteFloat f m (ChannelFloat name (ContinuousData shader)) = ChannelFloat name (ContinuousData shader')
  where
    Shader (Grid h' w') _ = shader
    (h,w) = unpackAcc (h',w')
    matte = Matte.matteToContinuous h w m
    shader' = applyToShader f matte shader

-- looks strange, but is necessary because of the weird accelerate behaviour
maskedApp :: (IsNum a, A.Elt a) => (A.Exp a -> A.Exp a) -> A.Exp a -> A.Exp a -> A.Exp a
maskedApp f a b = (a A.==* 0) A.? (b,b + a*(delta f b))
-- won't work, no idea what is the reason of that
--maskedApp f a b = b + a*(delta f b)
  where
    delta :: (IsNum a, A.Elt a) => (A.Exp a -> A.Exp a) -> (A.Exp a) -> (A.Exp a)
    delta f x = (f x) - x

applyToShader :: (IsNum b, A.Elt a, A.Elt b) => (A.Exp b -> A.Exp b) -> CartesianShader (A.Exp a) (A.Exp b) -> CartesianShader (A.Exp a) (A.Exp b) -> CartesianShader (A.Exp a) (A.Exp b)
applyToShader f matte mat = combineWith (maskedApp f) matte mat

applyToMatrix :: (IsNum a, A.Elt a) => (A.Exp a -> A.Exp a) -> Matrix2 a -> Matrix2 a -> Matrix2 a
applyToMatrix f matte mat = (M.zipWith (\x -> \y -> (maskedApp f x y)) matte) mat

offsetMatteLuna :: Color.RGBA Double -> Maybe (Matte.Matte Double) -> Image -> Image
offsetMatteLuna x@(fmap variable -> Color.RGBA r g b a) matte img =
  case matte of
    Nothing -> onEachRGBA (offset r) (offset g) (offset b) (offset a) img
    Just m ->
        onEachRGBAChannels (applyMatteFloat (offset r) m)
                           (applyMatteFloat (offset g) m)
                           (applyMatteFloat (offset b) m)
                           (applyMatteFloat (offset a) m) img

contrastMatteLuna :: Color.RGBA Double -> Maybe (Matte.Matte Double) -> Image -> Image
contrastMatteLuna x@(fmap variable -> Color.RGBA r g b a) matte img =
  case matte of
    Nothing -> onEachRGBA (contrast r) (contrast g) (contrast b) (contrast a) img
    Just m ->
        onEachRGBAChannels (applyMatteFloat (contrast r) m)
                           (applyMatteFloat (contrast g) m)
                           (applyMatteFloat (contrast b) m)
                           (applyMatteFloat (contrast a) m) img

exposureMatteLuna :: Color.RGBA Double -> Color.RGBA Double -> Maybe (Matte.Matte Double) -> Image -> Image
exposureMatteLuna x@(fmap variable -> Color.RGBA blackpointR blackpointG blackpointB blackpointA)
                  y@(fmap variable -> Color.RGBA exR exG exB exA) matte img =
                    case matte of
                      Nothing -> onEachRGBA (exposure blackpointR exR) (exposure blackpointG exG) (exposure blackpointB exB) id img -- (exposure blackpointA exA)
                      Just m ->
                          onEachRGBAChannels (applyMatteFloat (exposure blackpointR exR) m)
                                             (applyMatteFloat (exposure blackpointG exG) m)
                                             (applyMatteFloat (exposure blackpointB exB) m)
                                             (applyMatteFloat (exposure blackpointA exA) m) img

gradeLunaColorMatte :: VPS (Color.RGBA Double)
                    -> VPS (Color.RGBA Double)
                    -> VPS (Color.RGBA Double)
                    -> VPS (Color.RGBA Double)
                    -> Color.RGBA Double
                    -> Color.RGBA Double
                    -> Color.RGBA Double
                    -> Maybe (Matte.Matte Double)
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

offsetLuna :: Color.RGBA Double -> Image -> Image
offsetLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (offset r) (offset g) (offset b) (offset a)

contrastLuna :: Color.RGBA Double -> Image -> Image
contrastLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (contrast r) (contrast g) (contrast b) (contrast a)

exposureLuna :: Color.RGBA Double -> Color.RGBA Double -> Image -> Image
exposureLuna (fmap variable -> Color.RGBA blackpointR blackpointG blackpointB blackpointA)
             (fmap variable -> Color.RGBA exR exG exB exA) =
                 onEachRGBA (exposure blackpointR exR)
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

constantLuna :: Raster.Format -> Color.RGBA Double -> Image
constantLuna format {-- (variable -> width) (variable -> height) --} (fmap variable -> Color.RGBA r g b a) =
    case format of
        Raster.PCVideo       -> makeConst 640 480
        Raster.NTSC          -> makeConst 720 486
        Raster.PAL           -> makeConst 720 576
        Raster.HD            -> makeConst 1920 1080
        Raster.NTSC169       -> makeConst 720 486
        Raster.PAL169        -> makeConst 720 576
        Raster.K1Super35     -> makeConst 1024 778
        Raster.K1Cinemascope -> makeConst 914 778
        Raster.K2Super35     -> makeConst 2048 1556
        Raster.K2Cinemascope -> makeConst 1828 1556
        Raster.K4Super35     -> makeConst 4096 3112
        Raster.K4Cinemascope -> makeConst 3656 3112
        Raster.Square256     -> makeConst 256 256
        Raster.Square512     -> makeConst 512 512
        Raster.Square1K      -> makeConst 1024 1024
        Raster.Square2K      -> makeConst 2048 2048
        Raster.CustomFormat width height -> makeConst width height
        where   makeConst (variable -> width) (variable -> height) = 
                    Raster.constant (A.index2 width height) chans
                chans = [ ("rgba.r", r)
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

          gradientShader = Transform.scale (Grid width height) $ Transform.translate (V2 0.5 0.5) $ mapper gray gradient
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

          noiseShader = Transform.scale (Grid width height) noise

translateLuna :: V2 Double -> Image -> Image
translateLuna (fmap variable -> V2 x y) = onEachChannel translateChannel
    where v    = V2 x (-y)
          mask = Nothing
          translateChannel = \case
              (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
              (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
          transformation :: Point2 (Exp Double) -> Point2 (Exp Double)
          transformation pt = Transform.translate (strength pt) pt
          strength :: Point2 (Exp Double) -> V2 (Exp Double)
          strength pt = case mask of
              Nothing      -> v
              --TODO[KM]: handle the mask properly (aka. get rid of that ugly pattern match) and uncomment the other case option
              --          and keep in mind that applying mask for functions using a center point might cause the shader to extract the strength value from wrong mask's coordinates
              _ -> v
              --Just (VPS m) -> let
              --        Right rgba = Image.lookupPrimary m
              --        unpackMat (Right (Just (ChannelFloat _ (asMatrixData -> MatrixData c)))) = c -- TODO[KM]: this ugly pattern match :D
              --        m' = unpackMat $ View.get rgba "rgba.r"
              --        Shader _ str = Shader.nearest $ Shader.fromMatrix (A.Constant (0 :: Exp Double)) $ m'
              --        mult :: Point2 (Exp Double) -> Exp Double -> Exp Double
              --        mult pt x = str pt * x
              --    in (fmap (mult pt) v)

--adjustCoordinates :: Point2 (A.Exp Double) -> A.Exp Double -> Point2 (A.Exp Double)
--adjustCoordinates (Point2 x y) = Point2 ((A.intToFloat h) - x) y 

rotateLuna :: Double -> Image -> Image
rotateLuna (variable -> phi) = onEachChannel rotateChannel
    where mask = Nothing
          rotateChannel = \case
              (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
              (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
          transformation :: Point2 (Exp Double) -> Point2 (Exp Double)
          transformation pt = Transform.rotate (strength pt) pt
          strength :: Point2 (Exp Double) -> Exp Double
          strength pt = case mask of
              Nothing -> phi
              --TODO[KM]: handle the mask properly
              _       -> phi


rotateAtLuna :: Point2 Double -> Double -> Image -> Image
rotateAtLuna (fmap variable -> (Point2 x y)) (variable -> phi) = onEachChannel rotateChannel
    where vBefore = V2 x y
          vAfter  = V2 (-x) (-y)
          mask    = Nothing
          rotateChannel = \case
              (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
              (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
          transformation :: Point2 (Exp Double) -> Point2 (Exp Double)
          transformation pt = Transform.translate vAfter $ Transform.rotate (strength pt) $ Transform.translate vBefore pt
          strength :: Point2 (Exp Double) -> Exp Double
          strength pt = case mask of
              Nothing -> phi
              --TODO[KM]: handle the mask properly
              _       -> phi

scaleLuna :: V2 Double -> Image -> Image
scaleLuna (fmap variable -> v) = onEachChannel scaleChannel
    where mask = Nothing
          scaleChannel = \case
              (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
              (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData

          transformation :: Point2 (Exp Double) -> Point2 (Exp Double)
          transformation pt = Transform.scale (strength pt) pt

          strength :: Point2 (Exp Double) -> V2 (Exp Double)
          strength pt = case mask of
              Nothing -> v
              --TODO[KM]: handle the mask properly
              _       -> v



scaleAtLuna :: Point2 Double -> V2 Double -> Image -> Image
scaleAtLuna (fmap variable -> (Point2 x y)) (fmap variable -> v) = onEachChannel scaleChannel
    where vBefore = V2 x y
          vAfter  = V2 (-x) (-y)
          mask    = Nothing
          scaleChannel = \case
              (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
              (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
          transformation :: Point2 (Exp Double) -> Point2 (Exp Double)
          transformation pt = Transform.translate vAfter $ Transform.scale (strength pt) $ Transform.translate vBefore pt
          strength :: Point2 (Exp Double) -> V2 (Exp Double)
          strength pt = case mask of
              Nothing -> v
              --TODO[KM]: handle the mask properly
              _       -> v

--scaleToLuna :: A.Boundary (A.Exp Double) -> Int -> Int -> Image -> Image
--scaleToLuna boundary (variable -> x) (variable -> y) = onEachChannel $ rasterizer . monosampler . foo
--    where foo :: Matrix2 Double -> ContinuousShader (A.Exp Double)
--          foo = scale (Grid x y) . nearest . fromMatrix boundary

channelDim :: Channel -> (Int,Int)
channelDim = unpackAcc . channelDimAcc

channelDimAcc :: Channel -> (A.Exp Int, A.Exp Int)
channelDimAcc (ChannelFloat _ (MatrixData mat)) = (h,w)
  where
    sh = M.shape mat
    A.Z A.:. h A.:. w = A.unlift sh :: A.Z A.:. (A.Exp Int) A.:. (A.Exp Int)          

channelDimAcc (ChannelFloat _ (ContinuousData shader)) = (h,w)
  where
    Shader (Grid h w) _ = shader

channelDimAcc (ChannelFloat _ (DiscreteData shader)) = (h,w)
  where
    Shader (Grid h w) _ = shader


changeCoordinateSystem :: Channel -> Point2 (Exp Double) -> Point2 (Exp Double)
changeCoordinateSystem chan (Point2 x' y') = Point2 x' (h - y')
  where
    (h', _) = channelDimAcc chan
    h = A.fromIntegral h' :: Exp Double

processSkew :: Exp Double -> Exp Double -> SkewOrder -> Point2 (Exp Double) -> Point2 (Exp Double)
processSkew k k' order = case order of
                      SkewXY -> (Transform.verticalSkew k') . (Transform.horizontalSkew k)
                      SkewYX -> (Transform.horizontalSkew k) . (Transform.verticalSkew k')

strengthShader :: Maybe (Matte.Matte Double) -> Channel -> Point2 (Exp Double) -> Exp Double
strengthShader matte chan = case matte of
                              Nothing -> (\x -> 1)
                              Just m -> let (h,w) = channelDim chan
                                            Shader _ matteShader = Matte.matteToContinuous h w m
                                        in (\x -> (matteShader x))

rotateChannelAt :: Point2 Double -> Double -> Maybe (Matte.Matte Double) -> Channel -> Channel
rotateChannelAt (fmap variable -> (Point2 x y)) (variable -> phi) matte chan = (rotate chan)
    where
      vBefore = V2 (-x) (-y)
      vAfter  = V2 x y

      rotate = \case
          (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
          (Channel.asContinuous -> ChannelInt name zeData) -> ChannelInt name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData

      strength = strengthShader matte chan

      transformation :: Point2 (Exp Double) -> Point2 (Exp Double)
      transformation pt = (changeCoordinateSystem chan) $ Transform.translate vBefore $ Transform.rotate ((-phi)*(strength pt)) $ Transform.translate vAfter $ (changeCoordinateSystem chan) pt

scaleChannelAt :: Point2 Double -> V2 Double -> Maybe (Matte.Matte Double) -> Channel -> Channel
scaleChannelAt (fmap variable -> (Point2 x y)) (fmap variable -> v) matte chan = (scale chan)
    where
      vBefore = V2 (-x) (-y)
      vAfter  = V2 x y

      scale = \case
        (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
        (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData

      strength = strengthShader matte chan

      transformation :: Point2 (Exp Double) -> Point2 (Exp Double)
      transformation pt = (changeCoordinateSystem chan) $ Transform.translate vBefore $ Transform.scale (fmap (* (strength pt)) v) $ Transform.translate vAfter $ (changeCoordinateSystem chan) pt

translateChannel :: V2 Double -> Maybe (Matte.Matte Double) -> Channel -> Channel
translateChannel (fmap variable -> V2 x y) matte chan = (translate chan)
  where
    t = V2 x (-y)

    translate = \case
        (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
        (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData

    strength = strengthShader matte chan

    transformation :: Point2 (Exp Double) -> Point2 (Exp Double)
    transformation pt = Transform.translate (fmap (*(strength pt)) t) pt

skewChannelAt :: Point2 Double -> Skew Double -> Maybe (Matte.Matte Double) -> Channel -> Channel
skewChannelAt (fmap variable -> p) (Skew (fmap variable -> V2 k k') order) matte chan = (skew chan)
  where
    Point2 x y = changeCoordinateSystem chan p
    vBefore = V2 (-x) (-y)
    vAfter  = V2 x y

    skew = \case
      (Channel.asContinuous -> ChannelFloat name zeData) -> ChannelFloat name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData
      (Channel.asContinuous -> ChannelInt   name zeData) -> ChannelInt   name $ (\(ContinuousData shader) -> ContinuousData $ Shader.transform transformation shader) zeData

    strength = strengthShader matte chan

    transformation :: Point2 (Exp Double) -> Point2 (Exp Double)
    transformation pt = Transform.translate vBefore $ (processSkew (str * k) (str * k') order) $ Transform.translate vAfter pt
      where
        str = strength pt

-- temporary name
rotateAtMatteLuna :: Point2 Double -> Double -> Maybe (Matte.Matte Double) -> Image -> Image
rotateAtMatteLuna p ang matte = onEachChannel (rotateChannelAt p ang matte)

-- temporary name
scaleAtMatteLuna :: Point2 Double -> V2 Double -> Maybe (Matte.Matte Double) -> Image -> Image
scaleAtMatteLuna p v matte = onEachChannel (scaleChannelAt p v matte)

-- temporary name
translateMatteLuna :: V2 Double -> Maybe (Matte.Matte Double) -> Image -> Image
translateMatteLuna tr matte = onEachChannel (translateChannel tr matte)

-- temporary name
skewAtMatteLuna :: Point2 Double -> Skew Double -> Maybe (Matte.Matte Double) -> Image -> Image
skewAtMatteLuna p skew matte = onEachChannel (skewChannelAt p skew matte)

transformLuna :: Transform Double -> Maybe (Matte.Matte Double) -> Image -> Image
transformLuna tr matte = onEachChannel (transformChannel tr matte)
    where 
      transformChannel :: Transform Double -> Maybe (Matte.Matte Double) -> Channel -> Channel
      transformChannel (Transform tr phi sc skew ce) matte chan = (transformation chan)
        where
          transformation :: Channel -> Channel
          transformation = (translateChannel tr matte) . (rotateChannelAt ce phi matte) . (skewChannelAt ce skew matte) . (scaleChannelAt ce sc matte)

cropLuna :: Rectangle Int -> Image -> Image
cropLuna rect = onEachChannel cropChannel
    where cropChannel = \case
              ChannelFloat name zeData -> ChannelFloat name $ Transform.crop rect zeData
              ChannelInt   name zeData -> ChannelInt   name $ Transform.crop rect zeData

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
multiplyLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (*r) (*g) (*b) (*a)

gammaLuna :: Color.RGBA Double -> Image -> Image
gammaLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (gamma r) (gamma g) (gamma b) (gamma a)

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

type LunaHandleGUI = (VPS Int, VPS Double, VPS Double)
type LunaControlPointGUI a = (VPS (Point2 a), VPS LunaHandleGUI, VPS LunaHandleGUI)
type LunaCurveGUI a = [VPS (LunaControlPointGUI a)]

convertHandleGUI :: LunaHandleGUI -> CurveGUI.Handle
convertHandleGUI (unpackLunaVar -> t, unpackLunaVar -> w, unpackLunaVar -> a) =
    case t of
        0 -> CurveGUI.NonLinear w a
        1 -> CurveGUI.Vertical w
        2 -> CurveGUI.Linear

getValueAtCurveGUI :: LunaCurveGUI Double -> Double -> Double
getValueAtCurveGUI (convertCurveGUI -> curve) = CurveGUI.valueAtSpline curve

convertControlPointGUI :: LunaControlPointGUI a -> CurveGUI.ControlPoint a
convertControlPointGUI (unpackLunaVar -> p, unpackLunaVar -> hIn, unpackLunaVar -> hOut) =
    CurveGUI.ControlPoint p (convertHandleGUI hIn) (convertHandleGUI hOut)

convertCurveGUI :: LunaCurveGUI a -> CurveGUI.Curve a
convertCurveGUI (unpackLunaList -> c) = CurveGUI.BezierCurve (fmap convertControlPointGUI c)

hueCorrectLuna :: VPS (LunaCurveGUI Double) -> VPS (LunaCurveGUI Double) ->
                  VPS (LunaCurveGUI Double) -> VPS (LunaCurveGUI Double) -> VPS (LunaCurveGUI Double) ->
                  LunaCurveGUI Double -> LunaCurveGUI Double -> LunaCurveGUI Double ->
                  -- GUICurve Double -> sat_thrsh will be added later
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

type ColorCorrect a = (VPS (LunaCurveGUI a), VPS (LunaCurveGUI a))
pattern ColorCorrect a b = (VPS a, VPS b)

colorCorrectLunaCurves :: VPS (ColorCorrect Double)
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

colorCorrectLunaBase :: (BSpline Double, BSpline Double)
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
                          $ BSpline.valueAt (A.use curveShadows :: A.Acc (BSpline Double)) x
          strHighlights x = A.cond (x A.<=* 0.5) 0
                          $ A.cond (x A.>=* 1) 1
                          $ BSpline.valueAt (A.use curveHighlights :: A.Acc (BSpline Double)) x

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
    _          -> Left $ Image.ViewLookupError viewName

getChannelFromPrimaryLuna :: String -> Image -> Image.Result (Maybe Channel)
getChannelFromPrimaryLuna channelName img = case Image.lookupPrimary img of
    Right view -> View.get view channelName
    _          -> Left $ Image.ViewLookupError "primary view"

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
    constantLuna (Raster.CustomFormat 512 512) $ Color.RGBA (r*x) (g*x) (b*x) (a*x)
