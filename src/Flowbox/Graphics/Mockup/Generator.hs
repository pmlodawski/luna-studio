---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}

module Flowbox.Graphics.Mockup.Generator (
    Format(..),
    AnimableFloat(..),
    animableMaskFromBinary,
    animationFromBinary,
    circularLuna,
    conicalLuna,
    constantLuna,
    diamondLuna,
    formatMap,
    fromBinary,
    gradientLuna,
    interpolateMask,
    linearShapeLuna,
    maskFromBinary,
    radialShapeLuna,
    RotoAlgorithm(..),
    rotoLunaWrap,
    rotoLuna,
    rotoLunaL,
    rotoLunaB,
    squareLuna,
    unwrapFormat,
) where

import qualified Data.Array.Accelerate as A

import qualified Data.Binary               as Binary
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.Map                  as Map
import           Data.String               (fromString)
import           Linear                    (V2 (..))
import           Math.Coordinate           (Cartesian)
import           Math.Coordinate.Cartesian (Point2 (..))
import           Math.Metric               (Metric, MetricCoord)
import           Math.Space.Space          (Grid (..))

import           Flowbox.Geom2D.Mask                             (Mask (..))
import qualified Flowbox.Geom2D.Rasterizer                       as Rasterizer
import qualified Flowbox.Graphics.Color.Color                    as Color
import           Flowbox.Graphics.Composition.Generator.Gradient (Tick (..))
import qualified Flowbox.Graphics.Composition.Generator.Gradient as Gradient
import qualified Flowbox.Graphics.Composition.Generator.Raster   as Raster
import qualified Flowbox.Graphics.Composition.Transform          as Transform
import qualified Flowbox.Graphics.Image.Channel                  as Channel
import           Flowbox.Graphics.Image.Image                    (Image)
import qualified Flowbox.Graphics.Image.Image                    as Image
import qualified Flowbox.Graphics.Shader.Rasterizer              as Shader
import qualified Flowbox.Graphics.Shader.Sampler                 as Sampler
import           Flowbox.Graphics.Shader.Shader                  (Shader (..))
import           Flowbox.Graphics.Utils.Accelerate               (variable)
import qualified Flowbox.Graphics.Utils.Utils                    as U
import qualified Flowbox.Math.Matrix                             as M
import           Flowbox.Prelude                                 as P hiding (lookup)
-- experimental ---------
import Data.Binary
import Flowbox.Math.Function.CurveGUI (CurveGUI (..), valueAtSpline)
-------------------------
import Flowbox.Graphics.Mockup.Basic        as Basic
import Flowbox.Graphics.Mockup.ColorCorrect
import Flowbox.Graphics.Mockup.Matte

-- FIXME[KM -> *]: this should not be in mockup but in a designated module (separate one or the Composition.Generator), we might want to use it in pure haskell too
data Format = PCVideo
            | NTSC
            | PAL
            | HD
            | NTSC169
            | PAL169
            | K1Super35
            | K1Cinemascope
            | K2Super35
            | K2Cinemascope
            | K4Super35
            | K4Cinemascope
            | Square256
            | Square512
            | Square1K
            | Square2K
            | CustomFormat Int Int deriving(Eq, Ord)

formatMap :: Map.Map Format (Int,Int)
formatMap = Map.fromList ([(PCVideo, (640, 480))
                         ,(NTSC, (720, 486))
                         ,(PAL, (720, 576))
                         ,(HD, (1920, 1080))
                         ,(NTSC169, (720, 486))
                         ,(PAL169, (720, 576))
                         ,(K1Super35, (1024, 778))
                         ,(K1Cinemascope, (914, 778))
                         ,(K2Super35, (2048, 1556))
                         ,(K2Cinemascope, (1828, 1556))
                         ,(K4Super35, (4096, 3112))
                         ,(K4Cinemascope, (3656, 3112))
                         ,(Square256, (256, 256))
                         ,(Square512, (512, 512))
                         ,(Square1K, (1024, 1024))
                         ,(Square2K, (2048, 2048))
                         --,(CustomFormat x y, ( x, y))
                         ] :: [(Format,(Int,Int))] )

unwrapFormat :: Format -> (Int,Int)
unwrapFormat (CustomFormat w h) = (w, h)
unwrapFormat f = formatMap Map.! f

constantLuna :: Format -> Color.RGBA Float -> Image
constantLuna format {-- (variable -> width) (variable -> height) --} (fmap variable -> Color.RGBA r g b a) =
    case format of
        CustomFormat width height -> makeConst width height
        _ -> makeConst' $ formatMap Map.! format
        --Raster.PCVideo       -> makeConst 640 480
        --Raster.NTSC          -> makeConst 720 486
        --Raster.PAL           -> makeConst 720 576
        --Raster.HD            -> makeConst 1920 1080
        --Raster.NTSC169       -> makeConst 720 486
        --Raster.PAL169        -> makeConst 720 576
        --Raster.K1Super35     -> makeConst 1024 778
        --Raster.K1Cinemascope -> makeConst 914 778
        --Raster.K2Super35     -> makeConst 2048 1556
        --Raster.K2Cinemascope -> makeConst 1828 1556
        --Raster.K4Super35     -> makeConst 4096 3112
        --Raster.K4Cinemascope -> makeConst 3656 3112
        --Raster.Square256     -> makeConst 256 256
        --Raster.Square512     -> makeConst 512 512
        --Raster.Square1K      -> makeConst 1024 1024
        --Raster.Square2K      -> makeConst 2048 2048

        where   makeConst' = P.uncurry makeConst
                makeConst (variable -> width) (variable -> height) =
                    Raster.constant (A.index2 height width) chans
                chans = [ ("rgba.r", r)
                      , ("rgba.g", g)
                      , ("rgba.b", b)
                      , ("rgba.a", a)
                      ]

--constantLuna :: Int -> Int -> Color.RGBA Float -> Image
--constantLuna (variable -> width) (variable -> height) (fmap variable -> Color.RGBA r g b a) =
--    Raster.constant (A.index2 height width) chans
--    where chans = [ ("rgba.r", r)
--                  , ("rgba.g", g)
--                  , ("rgba.b", b)
--                  , ("rgba.a", a)
--                  ]

--TODO[KM]: port checkerboard to luna
--type CheckerboardColorsLuna = (VPS ColorD, VPS ColorD, VPS ColorD, VPS ColorD)
--type CheckerboardLineLuna   = (VPS ColorD, VPS Double)
-- ...
--checkerboardLuna :: VPS Int -> VPS Int -> Double -> CheckerboardColorsLuna -> CheckerboardLineLuna -> CheckerboardLineLuna -> Image
--checkerboardLuna w h

circularLuna :: Int -> Int -> Image
circularLuna = gradientLuna Gradient.circularShape

conicalLuna :: Int -> Int -> Image
conicalLuna = gradientLuna Gradient.conicalShape

squareLuna :: Int -> Image
squareLuna side = gradientLuna Gradient.squareShape side side

diamondLuna :: Int -> Int -> Image
diamondLuna = gradientLuna Gradient.diamondShape

radialShapeLuna :: (Metric a (Point2 (A.Exp Float)) (A.Exp Float), MetricCoord a Cartesian)
                => a -> Int -> Int -> Image
radialShapeLuna metric w h = gradientLuna (Gradient.radialShape metric) w h

linearShapeLuna :: Int -> Int -> Image
linearShapeLuna = gradientLuna Gradient.linearShape

gradientLuna :: (A.Lift A.Exp e, A.Plain e ~ Int)
             => Shader (Point2 (A.Exp Float)) (A.Exp Float) -> e -> e -> Image
gradientLuna gradient (variable -> width) (variable -> height) = channelToImageRGBA grad
    where grad = Shader.rasterizer $ Sampler.monosampler $ gradientShader

          gradientShader = Transform.scale (Grid width height) $ Transform.translate (V2 0.5 0.5) $ mapper gray gradient
          gray   = [Tick 0.0 0.0 1.0, Tick 1.0 1.0 1.0] :: [Tick Float Float Float]

          weightFun tickPos val1 _ val2 _ = U.mix tickPos val1 val2
          mapper = flip Gradient.colorMapper weightFun

--rotoLuna :: (Real a, Fractional a) => Image -> Mask a -> Format -> Bool -> Image
--rotoLuna input mask format premult = (if premult then premultiplyLuna else id) $
--    if Image.null input
--    then
--        let (w, h) = unwrapFormat format
--        in Rasterizer.matrixToImage $ Rasterizer.rasterizeMask w h mask
--    else
--        let Right sampleChan = Image.getFromPrimary "rgba.r" input
--            (w, h)           = Basic.unpackAccDims $ maybe (0, 0) Channel.size sampleChan
--            Right (Just a)   = Image.getFromPrimary "rgba.a" $ Rasterizer.matrixToImage $ Rasterizer.rasterizeMask w h mask
--        in Image.appendToPrimary a input

data RotoAlgorithm = GPU | CPU
    deriving (Show, Generic)

rotoLunaWrap :: Image -> Mask Float -> Format -> Bool -> Bool -> RotoAlgorithm -> Image
rotoLunaWrap input mask format premult premultAlpha alg = case alg of
    GPU -> rotoLuna input mask format premult premultAlpha
    CPU -> rotoLunaL input mask format premult premultAlpha

rotoLuna :: Image -> Mask Float -> Format -> Bool -> Bool -> Image
rotoLuna input mask format premult premultAlpha = (if premult then premultiplyLuna else id) $
    if Image.null input
    then
        let (w, h) = unwrapFormat format
        in Rasterizer.matrixToImage $ Rasterizer.rasterizeMask w h mask
    else
        let Right (Just aInput) = Image.getFromPrimary "rgba.a" input
            (w, h)              = Basic.unpackAccDims $ maybe (0, 0) Channel.size $ Just aInput
            aMatrix             = Rasterizer.rasterizeMask w h mask
            --a                   = Channel.ChannelFloat "rgba.a" $ Channel.MatrixData $ Rasterizer.rasterizeMask w h mask
            a = intersectOrOverWrite premultAlpha aMatrix $ Channel.asMatrix aInput
                where
                    intersectOrOverWrite :: Bool -> M.Matrix2 Float -> Channel.Channel -> Channel.Channel
                    intersectOrOverWrite True mat1 (Channel.ChannelFloat name (Channel.MatrixData mat2)) = Channel.ChannelFloat name $ Channel.MatrixData $ M.zipWith (*) mat1 mat2
                    intersectOrOverWrite False mat1 _ = Channel.ChannelFloat "rgba.a" $ Channel.MatrixData mat1
                    --fuck mat1 (ChannelInt name (MatrixData mat2)) = ChannelInt   name $ MatrixData $ M.zipWith (\a b -> a*b) mat1 mat2
        in Image.appendToPrimary a input

rotoLunaL :: Image -> Mask Float -> Format -> Bool -> Bool -> Image
rotoLunaL input mask format premult premultAlpha = (if premult then premultiplyLuna else id) $
    if Image.null input
    then
        let (w, h) = unwrapFormat format
        in Rasterizer.matrixToImage $ Rasterizer.rasterizeMaskL w h mask
    else
        let Right (Just aInput) = Image.getFromPrimary "rgba.a" input
            (w, h)              = Basic.unpackAccDims $ maybe (0, 0) Channel.size $ Just aInput
            aMatrix             = Rasterizer.rasterizeMask w h mask
            --a                   = Channel.ChannelFloat "rgba.a" $ Channel.MatrixData $ Rasterizer.rasterizeMask w h mask
            a = intersectOrOverWrite premultAlpha aMatrix $ Channel.asMatrix aInput
                where 
                    intersectOrOverWrite :: Bool -> M.Matrix2 Float -> Channel.Channel -> Channel.Channel
                    intersectOrOverWrite True mat1 (Channel.ChannelFloat name (Channel.MatrixData mat2)) = Channel.ChannelFloat name $ Channel.MatrixData $ M.zipWith (*) mat1 mat2
                    intersectOrOverWrite False mat1 _ = Channel.ChannelFloat "rgba.a" $ Channel.MatrixData mat1
                    --fuck mat1 (ChannelInt name (MatrixData mat2)) = ChannelInt   name $ MatrixData $ M.zipWith (\a b -> a*b) mat1 mat2
        in Image.appendToPrimary a input

-- experimental ---------

rotoLunaB :: Image -> String -> Format -> Bool -> Bool -> Image
rotoLunaB input mask = rotoLuna input (Binary.decode (fromString mask) :: Mask Float)


fromBinary :: Binary.Binary a => String -> a
fromBinary = Binary.decode . fromString

maskFromBinary :: String -> Mask Float
maskFromBinary = fromBinary

animationFromBinary :: String -> CurveGUI Float
animationFromBinary = fromBinary

animableMaskFromBinary :: String -> Mask AnimableFloat
animableMaskFromBinary = fromBinary

data AnimableFloat = ConstantValue Float
                   | AnimationCurve (CurveGUI Float)
                   deriving (Generic, Show)

instance Binary AnimableFloat

interpolateMask :: Mask AnimableFloat -> Float -> Mask Float
interpolateMask mask t = fmap (f t) mask
    where
        f _ (ConstantValue v)       = v
        f t (AnimationCurve curve)  = valueAtSpline curve t


-------------------------
