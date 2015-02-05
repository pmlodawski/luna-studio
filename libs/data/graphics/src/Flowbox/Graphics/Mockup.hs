---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Graphics.Mockup (
    -- == GENERAL

    ---- Data.Array.Accelerate
    --Boundary(..),
    -- Linear
    V2(..),
    ---- Math.Coordinate.Cartesian
    --Point2(..),
    -- Math.Metric
    Chebyshev(..),
    Euclidean(..),
    Minkowski(..),
    Taxicab(..),

    ---- == FLOWBOX

    ---- Flowbox.Geom2D.Rectangle
    --Rectangle(..),
    ---- Flowbox.Graphics.Color.Color
    ---- Flowbox.Graphics.Color.Companding.*
    --module Color,

    ---- == MOCKUP
    ---- Flowbox.Graphics.Mockup.Transform
    --Skew(..),
    --SkewOrder(..),
    --Transform(..),
) where

--import Data.Array.Accelerate     (Boundary(..))
import Linear                    (V2 (..))
--import Math.Coordinate.Cartesian (Point2 (..))
import Math.Metric               (Chebyshev(..), Euclidean(..), Minkowski(..), Taxicab(..))

--import Flowbox.Geom2D.Rectangle (Rectangle(..))
--import Flowbox.Graphics.Color.Color as Color

--import Flowbox.Graphics.Color.Companding.AlexaV3LogC as Color
--import Flowbox.Graphics.Color.Companding.Cineon      as Color
--import Flowbox.Graphics.Color.Companding.Gamma       as Color
--import Flowbox.Graphics.Color.Companding.LStar       as Color
--import Flowbox.Graphics.Color.Companding.Panalog     as Color
--import Flowbox.Graphics.Color.Companding.PLogLin     as Color
--import Flowbox.Graphics.Color.Companding.Rec709      as Color
--import Flowbox.Graphics.Color.Companding.REDLog      as Color
--import Flowbox.Graphics.Color.Companding.SLog        as Color
--import Flowbox.Graphics.Color.Companding.SRGB        as Color
--import Flowbox.Graphics.Color.Companding.ViperLog    as Color

--import Flowbox.Graphics.Mockup.Transform (Skew(..), SkewOrder(..), Transform(..))
