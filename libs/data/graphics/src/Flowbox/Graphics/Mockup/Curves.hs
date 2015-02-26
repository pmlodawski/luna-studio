---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module Flowbox.Graphics.Mockup.Curves (
    LunaCurveGUI,
    convertCurveGUI,
    getValueAtCurveGUI,
) where

import Math.Coordinate.Cartesian (Point2 (..))

import           Data.RTuple                    (RTuple (RTuple), toTuple)
import qualified Flowbox.Math.Function.CurveGUI as CurveGUI
import           Flowbox.Prelude                as P hiding (lookup)

import Flowbox.Graphics.Mockup.Basic



type LunaHandleGUI = RTuple (  VPS Int
                            , (VPS Float
                            , (VPS Float
                            , ())))

type LunaControlPointGUI a = RTuple (  VPS (Point2 a)
                                    , (VPS LunaHandleGUI
                                    , (VPS LunaHandleGUI
                                    , ())))
type LunaCurveGUI a = [LunaControlPointGUI a]


convertHandleGUI :: LunaHandleGUI -> CurveGUI.Handle
convertHandleGUI (toTuple -> (unpackLunaVar -> t, unpackLunaVar -> w, unpackLunaVar -> a)) =
    case t of
        0 -> CurveGUI.NonLinear w a
        1 -> CurveGUI.Vertical w
        2 -> CurveGUI.Linear

getValueAtCurveGUI :: LunaCurveGUI Float -> Float -> Float
getValueAtCurveGUI (convertCurveGUI -> curve) = CurveGUI.valueAtSpline curve

convertControlPointGUI :: LunaControlPointGUI a -> CurveGUI.ControlPoint a
convertControlPointGUI (toTuple -> (unpackLunaVar -> p, unpackLunaVar -> hIn, unpackLunaVar -> hOut)) =
    CurveGUI.ControlPoint p (convertHandleGUI hIn) (convertHandleGUI hOut)

convertCurveGUI :: LunaCurveGUI a -> CurveGUI.Curve a
convertCurveGUI c = CurveGUI.BezierCurve (fmap convertControlPointGUI c)
