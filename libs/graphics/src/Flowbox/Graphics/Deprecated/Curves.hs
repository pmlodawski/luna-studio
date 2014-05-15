---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Deprecated.Curves where

import Flowbox.Prelude as P


class Curve c where
    interpolate :: (Floating a) => c a -> a -> Point2D a

type Point2D a = (a, a)

data Bezier a = Bezier { begin       :: Point2D a
                       , handleBegin :: Point2D a
                       , handleEnd   :: Point2D a
                       , end         :: Point2D a
                       } deriving (Show)

instance Curve Bezier where
    interpolate curve t = (x, y) where
        x = calculate xA xB xC xD
        y = calculate yA yB yC yD
        calculate a b c d = a*(1-t)^3 + 3*b*t*(1-t)^2 + 3*c*t^2*(1-t) + d*t^3
        (xA, yA) = begin       curve
        (xB, yB) = handleBegin curve
        (xC, yC) = handleEnd   curve
        (xD, yD) = end         curve

-- TODO: research vector libraries for haskell + OpenVG
