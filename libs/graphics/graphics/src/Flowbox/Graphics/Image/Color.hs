---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternGuards #-}

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

import qualified Flowbox.Graphics.Utils                             as U
import           Flowbox.Math.Matrix                                as M
import           Flowbox.Prelude                                    as P



offset :: (Elt a, IsNum a) => Exp a -> Exp a -> Exp a
offset v = (+v)

multiply :: (Elt a, IsNum a) => Exp a -> Exp a -> Exp a
multiply v = (*v)

contrast :: (Elt a, IsFloating a) => Exp a -> Exp a -> Exp a
contrast v x = (x - 0.5) * v + 0.5


-- Possible to do with colorMapper
--clipTest :: (Elt a, IsScalar a, IsFloating a) => Generator (Exp a) -> Generator (Exp a)
--clipTest gen = Generator $ \p s ->
--    let pixel = runGenerator gen p s
--    in (pixel A.>* 1.0) A.? (runGenerator stripes p s, (pixel A.<* 0.0) A.? (runGenerator stripes p s, pixel))
