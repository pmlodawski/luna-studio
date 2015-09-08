---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Particle.Drag (
    Drag(..)
  , runDrag
) where

import           Control.Lens
import qualified Data.Array.Accelerate as A

import Particle
import Particle.Algebra

data Drag = Drag Float

runDrag :: Drag -> PipelinedSystem
runDrag (Drag coeff) ps = A.lift (timediff, pos, vel, newAccelerations, age)
  where
    (timediff, pos, vel, acc, age) = A.unlift ps :: UnliftedPipelinedSystem

    newAccelerations  = acc .+ calculatedForces
    calculatedForces  = A.map f vel
    f velocity        = negateVec3 (velocity `mulByScalar` velocityMultiplier)
      where
        velocityMultiplier = -1 * exp (-1 * (magnitude velocity / (A.the . A.unit . A.constant) coeff)) + 1
