---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Particle.Force (
    Force(..)
  , runForce
) where

import qualified Data.Array.Accelerate as A

import Particle
import Particle.Algebra

data Force = Force Vec3

runForce :: Force -> PipelinedSystem
runForce (Force f) ps = A.lift (timediff, pos, vel, newAccelerations, age)
  where
    (timediff, pos, vel, acc, age) = A.unlift ps :: UnliftedPipelinedSystem
    newAccelerations = A.map (`addVec3` fExp) acc
    fExp     = A.the . A.unit . A.constant $ f
