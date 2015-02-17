---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE ViewPatterns     #-}

module Flowbox.Graphics.Mockup.ColorCorrect (
    clampLuna,
    colorCorrectLuna,
    colorCorrectLunaCurves,
    contrastLuna,
    contrastMatteLuna,
    exposureLuna,
    exposureMatteLuna,
    gammaFromLinearLuna,
    gammaLuna,
    gammaToLinearLuna,
    gradeLunaColor,
    gradeLunaColorMatte,
    hsvToolLuna,
    hueCorrectLuna,
    invertLuna,
    multiplyLuna,
    offsetLuna,
    offsetMatteLuna,
    premultiplyLuna,
    saturateLuna,
    unpremultiplyLuna,
) where

import qualified Data.Array.Accelerate     as A
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



type ColorD = Color.RGBA Float
pattern ColorD r g b a = Color.RGBA r g b a
type Color5 = (VPS ColorD, VPS ColorD, VPS ColorD, VPS ColorD, VPS ColorD)

offsetMatteLuna :: Color.RGBA Float -> Maybe (Matte.Matte Float) -> Image -> Image
offsetMatteLuna x@(fmap variable -> Color.RGBA r g b a) matte img =
  case matte of
    Nothing -> onEachRGBA (CC.offset r) (CC.offset g) (CC.offset b) (CC.offset a) img
    Just m ->
        onEachRGBAChannels (applyMatteFloat (CC.offset r) m)
                           (applyMatteFloat (CC.offset g) m)
                           (applyMatteFloat (CC.offset b) m)
                           (applyMatteFloat (CC.offset a) m) img

contrastMatteLuna :: Color.RGBA Float -> Maybe (Matte.Matte Float) -> Image -> Image
contrastMatteLuna x@(fmap variable -> Color.RGBA r g b a) matte img =
  case matte of
    Nothing -> onEachRGBA (CC.contrast r) (CC.contrast g) (CC.contrast b) (CC.contrast a) img
    Just m ->
        onEachRGBAChannels (applyMatteFloat (CC.contrast r) m)
                           (applyMatteFloat (CC.contrast g) m)
                           (applyMatteFloat (CC.contrast b) m)
                           (applyMatteFloat (CC.contrast a) m) img

exposureMatteLuna :: Color.RGBA Float -> Color.RGBA Float -> Maybe (Matte.Matte Float) -> Image -> Image
exposureMatteLuna x@(fmap variable -> Color.RGBA blackpointR blackpointG blackpointB blackpointA)
                  y@(fmap variable -> Color.RGBA exR exG exB exA) matte img =
                    case matte of
                      Nothing -> onEachRGBA (CC.exposure blackpointR exR) (CC.exposure blackpointG exG) (CC.exposure blackpointB exB) id img -- (CC.exposure blackpointA exA)
                      Just m ->
                          onEachRGBAChannels (applyMatteFloat (CC.exposure blackpointR exR) m)
                                             (applyMatteFloat (CC.exposure blackpointG exG) m)
                                             (applyMatteFloat (CC.exposure blackpointB exB) m)
                                             (applyMatteFloat (CC.exposure blackpointA exA) m) img

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
                        Nothing -> onEachRGBA (CC.grade blackpointR whitepointR liftR gainR multiplyR offsetR gammaR)
                                              (CC.grade blackpointG whitepointG liftG gainG multiplyG offsetG gammaG)
                                              (CC.grade blackpointB whitepointB liftB gainB multiplyB offsetB gammaB)
                                              id img -- (CC.grade blackpointA whitepointA liftA gainA multiplyA offsetA gammaA)
                        Just m -> onEachRGBAChannels (applyMatteFloat (CC.grade blackpointR whitepointR liftR gainR multiplyR offsetR gammaR) m)
                                                     (applyMatteFloat (CC.grade blackpointG whitepointG liftG gainG multiplyG offsetG gammaG) m)
                                                     (applyMatteFloat (CC.grade blackpointB whitepointB liftB gainB multiplyB offsetB gammaB) m)
                                                     (applyMatteFloat (CC.grade blackpointA whitepointA liftA gainA multiplyA offsetA gammaA) m) img

offsetLuna :: Color.RGBA Float -> Image -> Image
offsetLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (CC.offset r) (CC.offset g) (CC.offset b) id -- (CC.offset a)

contrastLuna :: Color.RGBA Float -> Image -> Image
contrastLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (CC.contrast r) (CC.contrast g) (CC.contrast b) id -- (CC.contrast a)

exposureLuna :: Color.RGBA Float -> Color.RGBA Float -> Image -> Image
exposureLuna (fmap variable -> Color.RGBA blackpointR blackpointG blackpointB blackpointA)
             (fmap variable -> Color.RGBA exR exG exB exA) =
                 onEachRGBA (CC.exposure blackpointR exR)
                            (CC.exposure blackpointG exG)
                            (CC.exposure blackpointB exB)
                            id -- (CC.exposure blackpointA exA)

gradeLuna :: VPS Float -> VPS Float -> VPS Float -> Float -> Float -> Float -> Float -> Image -> Image
gradeLuna (VPS (variable -> blackpoint))
          (VPS (variable -> whitepoint))
          (VPS (variable -> lift))
          (variable -> gain)
          (variable -> multiply')
          (variable -> offset')
          (variable -> gamma') =
            onEach $ CC.grade blackpoint whitepoint lift gain multiply' offset' gamma'

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

          Right view = Image.lookupPrimary img

          view' = insertChannelFloats view [
                    ("rgba.r", rSaturated)
                  , ("rgba.g", gSaturated)
                  , ("rgba.b", bSaturated)
                  ]

          saturated = Image.insertPrimary view' img

--hsvToolLuna :: VPS Float -> VPS Float -> VPS Float -> VPS Float
--            -> VPS Float -> VPS Float -> VPS Float -> VPS Float
--            -> VPS Float -> VPS Float -> VPS Float -> VPS Float
--            -> A.Exp (Color.RGB Float)
--            -> A.Exp (Color.RGB Float)
--hsvToolLuna (VPS (variable -> hueRangeStart)) (VPS (variable -> hueRangeEnd))
--            (VPS (variable -> hueRotation)) (VPS (variable -> hueRolloff))
--            (VPS (variable -> saturationRangeStart)) (VPS (variable -> saturationRangeEnd))
--            (VPS (variable -> saturationAdjustment)) (VPS (variable -> saturationRolloff))
--            (VPS (variable -> brightnessRangeStart)) (VPS (variable -> brightnessRangeEnd))
--            (VPS (variable -> brightnessAdjustment)) (VPS (variable -> brightnessRolloff)) =
--    A.lift1 (CC.hsvTool (A.lift $ CC.Range hueRangeStart hueRangeEnd) hueRotation hueRolloff
--                        (A.lift $ CC.Range saturationRangeStart saturationRangeEnd) saturationAdjustment saturationRolloff
--                        (A.lift $ CC.Range brightnessRangeStart brightnessRangeEnd) brightnessAdjustment brightnessRolloff :: Color.RGB (A.Exp Float) -> Color.RGB (A.Exp Float))

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
                                              ( A.lift $ CC.Range brightnessRangeStart brightnessRangeEnd ) brightnessAdjustment brightnessRolloff :: Color.RGB (A.Exp Float) -> Color.RGB (A.Exp Float)
                                  )) image


clampLuna :: (VPS Float, VPS Float) -> Maybe (VPS Float, VPS Float) -> Image -> Image
clampLuna (VPS (variable -> thLo), VPS (variable -> thHi)) clamps =
    case clamps of
        Just (VPS clampLo, VPS clampHi) -> onEach $ CC.clamp (CC.Range thLo thHi) $ Just $ CC.Range (variable clampLo) (variable clampHi)
        _                               -> onEach $ CC.clamp (CC.Range thLo thHi) Nothing

premultiplyLuna :: Image -> Image
premultiplyLuna img = (*) `withAlpha` img

unpremultiplyLuna :: Image -> Image
unpremultiplyLuna img = (/) `withAlpha` img

invertLuna :: Image -> Image
invertLuna = onEachRGBA CC.invert CC.invert CC.invert id

colorMatrixLuna :: ColorMatrix Color.RGB Float -> Image -> Image
colorMatrixLuna matrix = onEachColorRGB (A.lift1 $ (CC.colorMatrix :: ColorMatrix Color.RGB Float -> Color.RGB (A.Exp Float) -> Color.RGB (A.Exp Float)) matrix)

multiplyLuna :: Color.RGBA Float -> Image -> Image
multiplyLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (*r) (*g) (*b) (*a)

gammaLuna :: Color.RGBA Float -> Image -> Image
gammaLuna (fmap variable -> Color.RGBA r g b a) = onEachRGBA (CC.gamma r) (CC.gamma g) (CC.gamma b) (CC.gamma a)

type HueCorrect a = (VPS (LunaCurveGUI a),
                     VPS (LunaCurveGUI a),
                     VPS (LunaCurveGUI a),
                     VPS (LunaCurveGUI a),
                     VPS (LunaCurveGUI a),
                     VPS (LunaCurveGUI a),
                     VPS (LunaCurveGUI a),
                     VPS (LunaCurveGUI a))

hueCorrectLuna :: HueCorrect Float ->
                  -- GUICurve Double -> sat_thrsh will be added later
                  -- sat_thrsh affects only r,g,b and lum parameters
                  Image -> Image
hueCorrectLuna ( VPS (convertCurveGUI-> lum), VPS (convertCurveGUI -> sat)
               , VPS (convertCurveGUI -> r), VPS (convertCurveGUI-> g), VPS (convertCurveGUI -> b)
               , VPS (convertCurveGUI -> rSup), VPS (convertCurveGUI -> gSup), VPS (convertCurveGUI-> bSup)
               ) img = onEachColorRGB (CC.hueCorrect (CurveGUI.convertToBSpline lum)
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
             onEachRGBA (CC.grade blackpointR whitepointR liftR gainR multiplyR offsetR gammaR)
                        (CC.grade blackpointG whitepointG liftG gainG multiplyG offsetG gammaG)
                        (CC.grade blackpointB whitepointB liftB gainB multiplyB offsetB gammaB)
                        id -- (CC.grade blackpointA whitepointA liftA gainA multiplyA offsetA gammaA)

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

colorCorrectLuna :: Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                 -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                 -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                 -> Color5 -- Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double -> Color.RGBA Double
                 -> Image
                 -> Image
colorCorrectLuna = colorCorrectLunaBase (curveShadows, curveHighlights)
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
                                 id -- (CC.colorCorrect contrastA gammaA gainA offsetA) saturated
                                 saturated
    where
          strShadows x    = A.cond (x A.<=* 0) 1
                          $ A.cond (x A.>=* 0.09) 0
                          $ BSpline.valueAt (A.use curveShadows :: A.Acc (BSpline Float)) x
          strHighlights x = A.cond (x A.<=* 0.5) 0
                          $ A.cond (x A.>=* 1) 1
                          $ BSpline.valueAt (A.use curveHighlights :: A.Acc (BSpline Float)) x

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

          Right view = Image.lookupPrimary img

          view' = insertChannelFloats view [
                    ("rgba.r", rSaturated)
                  , ("rgba.g", gSaturated)
                  , ("rgba.b", bSaturated)
                  ]

          saturated = Image.singleton view' -- Image.update (const $ Just view') "rgba" img

gammaToLinearLuna :: Gamma.Companding a (A.Exp Float) => a -> Image -> Image
gammaToLinearLuna companding = onEach $ (Gamma.toLinear companding :: A.Exp Float -> A.Exp Float)

gammaFromLinearLuna :: Gamma.Companding a (A.Exp Float) => a -> Image -> Image
gammaFromLinearLuna companding = onEach $ Gamma.fromLinear companding

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
               = onEachRGBA (CC.grade blackpointR whitepointR liftR gainR multiplyR offsetR gammaR)
                            (CC.grade blackpointG whitepointG liftG gainG multiplyG offsetG gammaG)
                            (CC.grade blackpointB whitepointB liftB gainB multiplyB offsetB gammaB)
                            id -- (CC.grade blackpointA whitepointA liftA gainA multiplyA offsetA gammaA)
