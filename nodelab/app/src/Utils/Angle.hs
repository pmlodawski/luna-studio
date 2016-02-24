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

toAngle :: Vector2 Double -> Angle
toAngle (Vector2 0.0 0.0) = 0.0
toAngle (Vector2 x y) = normAngle $ atan2 y x

boundedAngle :: Double -> Int -> Vector2 Double -> Vector2 Double -> Double
boundedAngle centralAngle' count src dst = angle where
    span' = pi / (fromIntegral count) / 2.0 * 0.8
    centralAngle = centralAngle'
    minAngle = centralAngle - span'
    maxAngle = centralAngle + span'
    connAngle = toAngle (dst - src)
    diffAngle = (normAngle $ (centralAngle' - connAngle + pi)) - pi
    angle
        | diffAngle < (-span') = maxAngle
        | diffAngle > span'    = minAngle
        | otherwise            = connAngle
