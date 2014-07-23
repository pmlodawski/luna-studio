---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Image.Color (
    module Flowbox.Graphics.Image.Color,
    U.bias,
    U.clamp',
    U.clamp,
    U.gain,
    U.gamma,
    U.invert,
    U.mix
) where

import           Flowbox.Graphics.Color
import qualified Flowbox.Graphics.Utils as U
import           Flowbox.Math.Matrix    as M
import           Flowbox.Prelude        as P



offset :: (Elt a, IsNum a) => Exp a -> Exp a -> Exp a
offset v = (+v)

multiply :: (Elt a, IsNum a) => Exp a -> Exp a -> Exp a
multiply v = (*v)

contrast :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a
contrast v x = (x - 0.5) * v + 0.5

data Colorspace = Linear | Cineon

exposure :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a -> Exp a
exposure blackpoint ex pix = blackpointConvert blackpoint (2 ** ex * (inverseBlackpointConvert blackpoint pix))

blackpointConvert :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a
blackpointConvert blackpoint pix = pointsConvert blackpoint 1.0 pix

inverseBlackpointConvert :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a
inverseBlackpointConvert lift pix = inversePointsConvert lift 1.0 pix

whitepointConvert :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a
whitepointConvert whitepoint pix = pointsConvert 0.0 whitepoint pix

inverseWhitepointConvert :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a
inverseWhitepointConvert gain pix = inversePointsConvert 0.0 gain pix

pointsConvert :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a -> Exp a
pointsConvert blackpoint whitepoint pix = (pix - blackpoint) / (whitepoint - blackpoint)

inversePointsConvert :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a -> Exp a
inversePointsConvert lift gain pix = (gain - lift) * pix + lift

grade :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a -> Exp a -> Exp a -> Exp a -> Exp a -> Exp a -> Exp a
grade blackpoint whitepoint lift gain multiply' offset' gamma =
	U.gamma gamma . offset offset' . multiply multiply' . inversePointsConvert lift gain . pointsConvert blackpoint whitepoint

colorCorrect :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a -> Exp a -> Exp a -> RGB (Exp a) -> RGB (Exp a)
colorCorrect saturation' contrast' gamma' gain' offset' pix =
    saturated & each %~ (offset offset' . gain'' gain' . U.gamma gamma' . contrast contrast')
    where saturated  = toHSV pix & s %~ (*saturation') & toRGB
          gain'' b x = b * x -- U.gain is broken, tested with Nuke that it's simply multiplication

-- NOTE[mm]: pretty sure Nuke uses HSL colorspace for saturation manipulation. There are slight differences still,
--           but operating on HSV looks unalike to Nuke.
saturate :: (Elt t, IsFloating t, ColorConvert a HSL, ColorConvert HSL a) => Exp t -> a (Exp t) -> a (Exp t)
saturate saturation pix = toHSL pix & s %~ (*saturation) & convertColor
