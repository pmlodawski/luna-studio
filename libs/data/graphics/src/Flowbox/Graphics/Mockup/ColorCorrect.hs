---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Mockup.ColorCorrect (
    ColorCorrectCurves(..),
    ColorCC(..),
    HueCorrectCurves(..),
    CrosstalkCurves(..),
    clampLuna,
    colorCorrectLuna,
    colorCorrectLunaCurves,
    contrastLuna,
    crosstalkLuna,
    exposureLuna,
    gammaFromLinearLuna,
    gammaLuna,
    gammaToLinearLuna,
    gradeLunaColor,
    hsvToolLuna,
    hueCorrectLuna,
    invertLuna,
    multiplyLuna,
    offsetLuna,
    premultiplyLuna,
    saturateLuna,
    unpremultiplyLuna,
) where

import qualified Data.Array.Accelerate     as A
import           Data.RTuple               (RTuple(..), toTuple)
import           Math.Coordinate.Cartesian (Point2 (..))

import qualified Flowbox.Graphics.Color.Color             as Color
import qualified Flowbox.Graphics.Color.Companding        as Gamma
import           Flowbox.Graphics.Composition.Color       (ColorMatrix)
import qualified Flowbox.Graphics.Composition.Color       as CC
import           Flowbox.Graphics.Image.Image             (Image)
import qualified Flowbox.Graphics.Image.Image             as Image
import qualified Flowbox.Graphics.Image.Matte             as Matte
import           Flowbox.Graphics.Utils.Accelerate        (variable)
import           Flowbox.Math.Function.Accelerate.BSpline as BSpline
import qualified Flowbox.Math.Function.CurveGUI           as CurveGUI
import           Flowbox.Math.Matrix                      as M
import           Flowbox.Prelude                          as P hiding (lookup)

import Flowbox.Graphics.Mockup.Basic
import Flowbox.Graphics.Mockup.Curves
import Flowbox.Graphics.Mockup.Matte



type ColorF = Color.RGBA Float
pattern ColorF r g b a = Color.RGBA r g b a

offsetLuna :: Color.RGBA Float -> Maybe (Matte.Matte Float) -> Image -> Image
offsetLuna (fmap variable -> Color.RGBA r g b a) matte img =
  case matte of
    Nothing -> onEachRGBA (CC.offset r) (CC.offset g) (CC.offset b) (CC.offset a) img
    Just m ->
        onEachRGBAChannels (applyMatteFloat (CC.offset r) m)
                           (applyMatteFloat (CC.offset g) m)
                           (applyMatteFloat (CC.offset b) m)
                           (applyMatteFloat (CC.offset a) m) img

contrastLuna :: Color.RGB Float -> Maybe (Matte.Matte Float) -> Image -> Image
contrastLuna (fmap variable -> Color.RGB r g b) matte img =
  case matte of
    Nothing -> onEachRGBA (CC.contrast r) (CC.contrast g) (CC.contrast b) id img
    Just m ->
        onEachRGBAChannels (applyMatteFloat (CC.contrast r) m)
                           (applyMatteFloat (CC.contrast g) m)
                           (applyMatteFloat (CC.contrast b) m)
                           id img

exposureLuna :: Color.RGB Float -> Color.RGB Float -> Maybe (Matte.Matte Float) -> Image -> Image
exposureLuna (fmap variable -> Color.RGB blackpointR blackpointG blackpointB)
                  (fmap variable -> Color.RGB exR exG exB) matte img =
                    case matte of
                      Nothing -> onEachRGBA (CC.exposure blackpointR exR) (CC.exposure blackpointG exG) (CC.exposure blackpointB exB) id img
                      Just m ->
                          onEachRGBAChannels (applyMatteFloat (CC.exposure blackpointR exR) m)
                                            (applyMatteFloat (CC.exposure blackpointG exG) m)
                                            (applyMatteFloat (CC.exposure blackpointB exB) m)
                                            id img

gradeLunaColor :: VPS (Color.RGBA Float)
               -> VPS (Color.RGBA Float)
               -> VPS (Color.RGBA Float)
               -> VPS (Color.RGBA Float)
               -> Color.RGBA Float
               -> Color.RGBA Float
               -> Color.RGBA Float
               -> Maybe (Matte.Matte Float)
               -> Image
               -> Image
gradeLunaColor (VPS (fmap variable -> Color.RGBA blackpointR blackpointG blackpointB blackpointA))
                    (VPS (fmap variable -> Color.RGBA whitepointR whitepointG whitepointB whitepointA))
                    (VPS (fmap variable -> Color.RGBA liftR liftG liftB liftA))
                    (VPS (fmap variable -> Color.RGBA gainR gainG gainB gainA))
                    (fmap variable -> Color.RGBA multiplyR multiplyG multiplyB multiplyA)
                    (fmap variable -> Color.RGBA offsetR offsetG offsetB offsetA)
                    (fmap variable -> Color.RGBA gammaR gammaG gammaB gammaA) matte img =
                      case matte of
                        Nothing -> onEachRGBA (CC.grade blackpointR whitepointR liftR gainR multiplyR offsetR gammaR)
                                              (CC.grade blackpointG whitepointG liftG gainG multiplyG offsetG gammaG)
                                              (CC.grade blackpointB whitepointB liftB gainB multiplyB offsetB gammaB)
                                              (CC.grade blackpointA whitepointA liftA gainA multiplyA offsetA gammaA) img
                        Just m -> onEachRGBAChannels (applyMatteFloat (CC.grade blackpointR whitepointR liftR gainR multiplyR offsetR gammaR) m)
                                                     (applyMatteFloat (CC.grade blackpointG whitepointG liftG gainG multiplyG offsetG gammaG) m)
                                                     (applyMatteFloat (CC.grade blackpointB whitepointB liftB gainB multiplyB offsetB gammaB) m)
                                                     (applyMatteFloat (CC.grade blackpointA whitepointA liftA gainA multiplyA offsetA gammaA) m) img

multiplyLuna :: Color.RGBA Float -> Maybe (Matte.Matte Float) -> Image -> Image
multiplyLuna (fmap variable -> Color.RGBA r g b a) matte img =
  case matte of
    Nothing -> onEachRGBA (*r) (*g) (*b) (*a) img
    Just m ->
        onEachRGBAChannels (applyMatteFloat (*r) m)
                           (applyMatteFloat (*g) m)
                           (applyMatteFloat (*b) m)
                           (applyMatteFloat (*a) m) img

type AffectAlpha = Bool

gammaLuna :: Color.RGBA Float -> AffectAlpha -> Maybe (Matte.Matte Float) -> Image -> Image
gammaLuna (fmap variable -> Color.RGBA r g b a) affectAlpha matte img =
  case matte of
    Nothing -> onEachRGBA (CC.gamma r) (CC.gamma g) (CC.gamma b) alpha img
    Just m ->
        onEachRGBAChannels (applyMatteFloat (CC.gamma r) m)
                           (applyMatteFloat (CC.gamma g) m)
                           (applyMatteFloat (CC.gamma b) m)
                           (applyMatteFloat alpha m) img
  where alpha = case affectAlpha of
                    True  -> (CC.gamma a)
                    False -> id

-- [TODO] - zamaskować saturate
saturateLuna :: Color.RGBA Float -> Image -> Image
saturateLuna (fmap variable -> Color.RGBA saturationR saturationG saturationB _) img = saturated
    where rgb = unsafeGetRGB img

          (rSaturated, gSaturated, bSaturated) = CC.saturateRGB saturationR saturationG saturationB rgb

          --rgbRsaturated = M.map (A.lift1 (saturateOnHSV saturationR)) rgb
          --rgbGsaturated = M.map (A.lift1 (saturateOnHSV saturationG)) rgb
          --rgbBsaturated = M.map (A.lift1 (saturateOnHSV saturationB)) rgb

          --saturateOnHSV :: Exp Float -> Color.RGB (Exp Float) -> Color.RGB (Exp Float)
          --saturateOnHSV sat pix = Color.toHSL pix & (\(Color.HSL h s l) -> Color.HSL h (s * sat) l) & Color.toRGB

          --rSaturated = M.map (\(A.unlift -> Color.RGB r _ _) -> r) rgbRsaturated
          --gSaturated = M.map (\(A.unlift -> Color.RGB _ g _) -> g) rgbGsaturated
          --bSaturated = M.map (\(A.unlift -> Color.RGB _ _ b) -> b) rgbBsaturated

          Right view = Image.lookupPrimary img

          view' = insertChannelFloats view [
                   ("rgba.r", rSaturated)
                  ,("rgba.g", gSaturated)
                  ,("rgba.b", bSaturated)
                  ]

          saturated = Image.insertPrimary view' img

--hsvToolLuna :: VPS Float -> VPS Float -> VPS Float -> VPS Float
--            -> VPS Float -> VPS Float -> VPS Float -> VPS Float
--            -> VPS Float -> VPS Float -> VPS Float -> VPS Float
--            -> Exp (Color.RGB Float)
--            -> Exp (Color.RGB Float)
--hsvToolLuna (VPS (variable -> hueRangeStart)) (VPS (variable -> hueRangeEnd))
--            (VPS (variable -> hueRotation)) (VPS (variable -> hueRolloff))
--            (VPS (variable -> saturationRangeStart)) (VPS (variable -> saturationRangeEnd))
--            (VPS (variable -> saturationAdjustment)) (VPS (variable -> saturationRolloff))
--            (VPS (variable -> brightnessRangeStart)) (VPS (variable -> brightnessRangeEnd))
--            (VPS (variable -> brightnessAdjustment)) (VPS (variable -> brightnessRolloff)) =
--    A.lift1 (CC.hsvTool (A.lift $ CC.Range hueRangeStart hueRangeEnd) hueRotation hueRolloff
--                        (A.lift $ CC.Range saturationRangeStart saturationRangeEnd) saturationAdjustment saturationRolloff
--                        (A.lift $ CC.Range brightnessRangeStart brightnessRangeEnd) brightnessAdjustment brightnessRolloff :: Color.RGB (Exp Float) -> Color.RGB (Exp Float))

hsvToolLuna :: VPS Float -> VPS Float -> VPS Float -> VPS Float
            -> VPS Float -> VPS Float -> VPS Float -> VPS Float
            -> VPS Float -> VPS Float -> VPS Float -> VPS Float
            -> VPS Image
            -> VPS Image
hsvToolLuna (VPS (variable -> hueRangeStart)) (VPS (variable -> hueRangeEnd))
            (VPS (variable -> hueRotation)) (VPS (variable -> hueRolloff))
            (VPS (variable -> saturationRangeStart)) (VPS (variable -> saturationRangeEnd))
            (VPS (variable -> saturationAdjustment)) (VPS (variable -> saturationRolloff))
            (VPS (variable -> brightnessRangeStart)) (VPS (variable -> brightnessRangeEnd))
            (VPS (variable -> brightnessAdjustment)) (VPS (variable -> brightnessRolloff))
            (VPS image) =
    VPS $ onEachColorRGB (A.lift1 (CC.hsvTool ( A.lift $ CC.Range hueRangeStart hueRangeEnd ) hueRotation hueRolloff
                                              ( A.lift $ CC.Range saturationRangeStart saturationRangeEnd ) saturationAdjustment saturationRolloff
                                              ( A.lift $ CC.Range brightnessRangeStart brightnessRangeEnd ) brightnessAdjustment brightnessRolloff :: Color.RGB (Exp Float) -> Color.RGB (Exp Float)
                                  )) image


clampLuna :: RTuple (VPS Float, (VPS Float, ())) -> Maybe (VPS Float, VPS Float) -> Image -> Image
clampLuna (toTuple -> (VPS (variable -> thLo), VPS (variable -> thHi))) clamps =
    case clamps of
        Just (VPS clampLo, VPS clampHi) -> onEach $ CC.clamp (CC.Range thLo thHi) $ Just $ CC.Range (variable clampLo) (variable clampHi)
        _                               -> onEach $ CC.clamp (CC.Range thLo thHi) Nothing

premultiplyLuna :: Image -> Image
premultiplyLuna img = (*) `withAlpha` img

unpremultiplyLuna :: Image -> Image
unpremultiplyLuna img = (/) `withAlpha` img

invertLuna :: Image -> Image
invertLuna = onEachRGBA CC.invert CC.invert CC.invert CC.invert

colorMatrixLuna :: ColorMatrix Color.RGB Float -> Image -> Image
colorMatrixLuna matrix = onEachColorRGB (A.lift1 $ (CC.colorMatrix :: ColorMatrix Color.RGB Float -> Color.RGB (Exp Float) -> Color.RGB (Exp Float)) matrix)

data HueCorrectCurves a = HueCorrectCurves { _sat   :: CurveGUI a
                                           , _lum   :: CurveGUI a
                                           , _red   :: CurveGUI a
                                           , _green :: CurveGUI a
                                           , _blue  :: CurveGUI a
                                           , _rsup  :: CurveGUI a
                                           , _gsup  :: CurveGUI a
                                           , _bsup  :: CurveGUI a
                                           }
                        deriving Show

hueCorrectLuna :: HueCorrectCurves Float ->
                  -- GUICurve Double -> sat_thrsh will be added later
                  -- sat_thrsh affects only r,g,b and lum parameters
                  Image -> Image
hueCorrectLuna (HueCorrectCurves sat lum r g b rSup gSup bSup) img =
    onEachColorRGB (CC.hueCorrect (CurveGUI.convertToBSpline lum)
                                  (CurveGUI.convertToBSpline sat)
                                  (CurveGUI.convertToBSpline r)
                                  (CurveGUI.convertToBSpline g)
                                  (CurveGUI.convertToBSpline b)
                                  (CurveGUI.convertToBSpline rSup)
                                  (CurveGUI.convertToBSpline gSup)
                                  (CurveGUI.convertToBSpline bSup)
                   ) img

data ColorCorrectCurves a = ColorCorrectCurves { _shadows    :: CurveGUI a
                                               , _highlights :: CurveGUI a
                                               }

data ColorCC a = ColorCC { _saturation :: a
                         , _contrast   :: a
                         , _gamma      :: a
                         , _gain       :: a
                         , _offset     :: a
                         }
    deriving (Show)

colorCorrectLunaCurves :: VPS (ColorCorrectCurves Float)
                       -> ColorCC ColorF
                       -> ColorCC ColorF
                       -> ColorCC ColorF
                       -> ColorCC ColorF
                       -> Image
                       -> Image
colorCorrectLunaCurves (VPS (ColorCorrectCurves curveShadows curveHighlights)) = colorCorrectLunaBase (prepare curveShadows, prepare curveHighlights)
    where prepare (BezierCurveGUI nodes) = let nodes' = CurveGUI.convertToNodeList nodes in A.fromList (Z :. length nodes') nodes'

colorCorrectLuna :: ColorCC ColorF
                 -> ColorCC ColorF
                 -> ColorCC ColorF
                 -> ColorCC ColorF
                 -> Image
                 -> Image
colorCorrectLuna = colorCorrectLunaBase (curveShadows, curveHighlights)
    where curveShadows    = makeSpline [BSplineNode (Point2 0 1) (Point2 (-1) 1) (Point2 0.03 1), BSplineNode (Point2 0.09 0) (Point2 0.06 0) (Point2 1.09 0)]
          curveHighlights = makeSpline [BSplineNode (Point2 0.5 0) (Point2 (-0.5) 0) (Point2 (2/3) 0), BSplineNode (Point2 1 1) (Point2 (5/6) 1) (Point2 2 1)]
          makeSpline      = A.fromList (Z :. 2)

pattern ColorCCV r g b a <- (fmap variable -> ColorF r g b a)

colorCorrectLunaBase :: (BSpline Float, BSpline Float)
                     -> ColorCC ColorF
                     -> ColorCC ColorF
                     -> ColorCC ColorF
                     -> ColorCC ColorF
                     -> Image
                     -> Image
colorCorrectLunaBase (curveShadows, curveHighlights)
                  ( ColorCC
                  ( ColorCCV masterSaturationR masterSaturationG masterSaturationB masterSaturationA )
                  ( ColorCCV masterContrastR masterContrastG masterContrastB masterContrastA )
                  ( ColorCCV masterGammaR masterGammaG masterGammaB masterGammaA )
                  ( ColorCCV masterGainR masterGainG masterGainB masterGainA )
                  ( ColorCCV masterOffsetR masterOffsetG masterOffsetB masterOffsetA )
                  )
                  ( ColorCC
                  ( ColorCCV shadowsSaturationR shadowsSaturationG shadowsSaturationB shadowsSaturationA )
                  ( ColorCCV shadowsContrastR shadowsContrastG shadowsContrastB shadowsContrastA )
                  ( ColorCCV shadowsGammaR shadowsGammaG shadowsGammaB shadowsGammaA )
                  ( ColorCCV shadowsGainR shadowsGainG shadowsGainB shadowsGainA )
                  ( ColorCCV shadowsOffsetR shadowsOffsetG shadowsOffsetB shadowsOffsetA )
                  )
                  ( ColorCC
                  ( ColorCCV midtonesSaturationR midtonesSaturationG midtonesSaturationB midtonesSaturationA )
                  ( ColorCCV midtonesContrastR midtonesContrastG midtonesContrastB midtonesContrastA )
                  ( ColorCCV midtonesGammaR midtonesGammaG midtonesGammaB midtonesGammaA )
                  ( ColorCCV midtonesGainR midtonesGainG midtonesGainB midtonesGainA )
                  ( ColorCCV midtonesOffsetR midtonesOffsetG midtonesOffsetB midtonesOffsetA )
                  )
                  ( ColorCC
                  ( ColorCCV highlightsSaturationR highlightsSaturationG highlightsSaturationB highlightsSaturationA )
                  ( ColorCCV highlightsContrastR highlightsContrastG highlightsContrastB highlightsContrastA )
                  ( ColorCCV highlightsGammaR highlightsGammaG highlightsGammaB highlightsGammaA )
                  ( ColorCCV highlightsGainR highlightsGainG highlightsGainB highlightsGainA )
                  ( ColorCCV highlightsOffsetR highlightsOffsetG highlightsOffsetB highlightsOffsetA )
                  )
                  = onEachColorRGBA correct
    where
          strShadows x    = BSpline.valueAt (A.use curveShadows :: A.Acc (BSpline Float)) x
          strHighlights x = BSpline.valueAt (A.use curveHighlights :: A.Acc (BSpline Float)) x

          correctMasterR = CC.colorCorrect masterContrastR masterGammaR masterGainR masterOffsetR
          correctMasterG = CC.colorCorrect masterContrastG masterGammaG masterGainG masterOffsetG
          correctMasterB = CC.colorCorrect masterContrastB masterGammaB masterGainB masterOffsetB

          correctShadowsR = CC.colorCorrect shadowsContrastR shadowsGammaR shadowsGainR shadowsOffsetR
          correctShadowsG = CC.colorCorrect shadowsContrastG shadowsGammaG shadowsGainG shadowsOffsetG
          correctShadowsB = CC.colorCorrect shadowsContrastB shadowsGammaB shadowsGainB shadowsOffsetB

          correctMidtonesR = CC.colorCorrect midtonesContrastR midtonesGammaR midtonesGainR midtonesOffsetR
          correctMidtonesG = CC.colorCorrect midtonesContrastG midtonesGammaG midtonesGainG midtonesOffsetG
          correctMidtonesB = CC.colorCorrect midtonesContrastB midtonesGammaB midtonesGainB midtonesOffsetB

          correctHighlightsR = CC.colorCorrect highlightsContrastR highlightsGammaR highlightsGainR highlightsOffsetR
          correctHighlightsG = CC.colorCorrect highlightsContrastG highlightsGammaG highlightsGainG highlightsOffsetG
          correctHighlightsB = CC.colorCorrect highlightsContrastB highlightsGammaB highlightsGainB highlightsOffsetB

          correct (A.unlift -> rgba :: Color.RGBA (Exp Float)) = let
                  hsl@(Color.HSL _ _ l) = Color.toHSL rgba
                  coeffShadows          = strShadows    l
                  coeffHighlights       = strHighlights l
                  coeffMidtones         = 1 - coeffShadows - coeffHighlights

                  -- TODO[KM]: think whether the saturation should be taken from HSL or HSV model, the lightness used for defining the strength for shadows and highlights is most likely taken from HSL
                  saturate :: Color.HSL (Exp Float) -> A.Exp Float -> A.Exp Float -> A.Exp Float -> A.Exp Float ->  Color.RGB (Exp Float)
                  saturate (Color.HSL h' s' l') masterSat shadowsSat midtonesSat highlightsSat = Color.toRGB $ Color.HSL h' (s' * masterSat * (coeffShadows * shadowsSat + coeffMidtones * midtonesSat + coeffHighlights * highlightsSat)) l'

                  cCorrect :: (Exp Float -> Exp Float) -> (Exp Float -> Exp Float) -> (Exp Float -> Exp Float) -> Exp Float -> Exp Float
                  cCorrect shadows midtones highlights x = coeffShadows * shadows x + coeffMidtones * midtones x + coeffHighlights * highlights x

                  Color.RGB rSat _ _   = saturate hsl masterSaturationR shadowsSaturationR midtonesSaturationR highlightsSaturationR
                  Color.RGB _ gSat _   = saturate hsl masterSaturationG shadowsSaturationG midtonesSaturationG highlightsSaturationG
                  Color.RGB _ _ bSat   = saturate hsl masterSaturationB shadowsSaturationB midtonesSaturationB highlightsSaturationB
                  Color.RGB (correctMasterR -> r) (correctMasterG -> g) (correctMasterB -> b) = Color.RGB rSat gSat bSat

                  r' = cCorrect correctShadowsR correctMidtonesR correctHighlightsR r
                  g' = cCorrect correctShadowsG correctMidtonesG correctHighlightsG g
                  b' = cCorrect correctShadowsB correctMidtonesB correctHighlightsB b
              in A.lift $ Color.RGBA r' g' b' $ rgba ^. Color.rgbaA

gammaToLinearLuna :: Gamma.Companding a (Exp Float) => a -> Image -> Image
gammaToLinearLuna companding = onEach $ (Gamma.toLinear companding :: Exp Float -> Exp Float)

gammaFromLinearLuna :: Gamma.Companding a (Exp Float) => a -> Image -> Image
gammaFromLinearLuna companding = onEach $ Gamma.fromLinear companding

data CrosstalkCurves a = CrosstalkCurves { _redC :: CurveGUI a
                                         , _greenC :: CurveGUI a
                                         , _blueC :: CurveGUI a
                                         , _redGreen :: CurveGUI a
                                         , _redBlue :: CurveGUI a
                                         , _greenRed :: CurveGUI a
                                         , _greenBlue :: CurveGUI a
                                         , _blueRed :: CurveGUI a
                                         , _blueGreen :: CurveGUI a
                                         }
                       deriving Show

crosstalkLuna :: CrosstalkCurves Float -> Image -> Image
crosstalkLuna (CrosstalkCurves r g b rg rb gr gb br bg) img =
    onEachColorRGB (CC.crosstalk (CurveGUI.convertToBSpline r)
                                 (CurveGUI.convertToBSpline g)
                                 (CurveGUI.convertToBSpline b)
                                 (CurveGUI.convertToBSpline rg)
                                 (CurveGUI.convertToBSpline rb)
                                 (CurveGUI.convertToBSpline gr)
                                 (CurveGUI.convertToBSpline gb)
                                 (CurveGUI.convertToBSpline br)
                                 (CurveGUI.convertToBSpline bg)
                   ) img
