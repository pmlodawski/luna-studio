module Utils.Angle where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Fixed

type Angle  = Double

normAngle :: Angle -> Angle
normAngle a = (2 * pi + a) `mod'` (2 * pi)

toRelAngle :: Angle -> Angle
toRelAngle a = if a > pi then (2 * pi) - a else a

angleDiff :: Angle -> Angle -> Angle
angleDiff a1 a2 = toRelAngle . normAngle $ a2 - a1

calcAngle :: Vector2 Double -> Vector2 Double -> Angle
calcAngle vecDest vecSrc = normAngle $ atan2 (vecDiff ^. y) (vecDiff ^. x) where
    vecDiff = vecDest - vecSrc
