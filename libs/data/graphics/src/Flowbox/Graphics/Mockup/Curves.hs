---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module Flowbox.Graphics.Mockup.Curves (
    CurveGUI(..),
    ControlPointGUI(..),
    HandleGUI(..),
    --convertCurveGUI,
    getValueAtCurveGUI,
) where

import           Data.RTuple                    (toTuple)
import           Flowbox.Math.Function.CurveGUI (ControlPointGUI (..), CurveGUI (..), HandleGUI (..))
import qualified Flowbox.Math.Function.CurveGUI as CurveGUI
import           Flowbox.Prelude                as P hiding (lookup)

import Flowbox.Graphics.Mockup.Basic



--convertHandleGUI :: LunaHandleGUI -> CurveGUI.Handle
--convertHandleGUI (toTuple -> (unpackLunaVar -> t, unpackLunaVar -> w, unpackLunaVar -> a)) =
--    case t of
--        0 -> CurveGUI.NonLinear w a
--        1 -> CurveGUI.Vertical w
--        2 -> CurveGUI.Linear

--convertControlPointGUI :: LunaControlPointGUI a -> CurveGUI.ControlPoint a
--convertControlPointGUI (toTuple -> (unpackLunaVar -> p, unpackLunaVar -> hIn, unpackLunaVar -> hOut)) =
--    CurveGUI.ControlPoint p (convertHandleGUI hIn) (convertHandleGUI hOut)

--convertCurveGUI :: LunaCurveGUI a -> CurveGUI.Curve a
--convertCurveGUI c = CurveGUI.BezierCurve (fmap convertControlPointGUI c)

getValueAtCurveGUI :: CurveGUI Float -> Float -> Float
getValueAtCurveGUI = CurveGUI.valueAtSpline
