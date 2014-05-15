module Particle.PointAttractor (
    PointAttractor(..)
  , runPointAttractor
  --, runPointAttractor'
) where

import Control.Lens
import qualified Data.Array.Accelerate as A

import Particle
import Particle.Algebra

data PointAttractor = PointAttractor Position Float

runPointAttractor :: PointAttractor -> PipelinedSystem
runPointAttractor (PointAttractor attractorPos attractorMass) ps = A.lift (timediff, pos, vel, newAccelerations, age)
  where
    (timediff, pos, vel, acc, age) = A.unlift ps :: UnliftedPipelinedSystem
    newAccelerations   = acc .+ forces
    forces             = A.map accelerationAtPosition pos

    accelerationAtPosition :: A.Exp Position -> A.Exp Acceleration
    accelerationAtPosition particlePosition = normalize distanceVector `mulByScalar`
      ((A.constant gravitationalConstant * (A.the . A.unit . A.constant) attractorMass) / (distance ** 2))
      where
        distance              = magnitude distanceVector
        distanceVector        = distanceVec3 (A.the . A.unit . A.constant $ attractorPos) particlePosition
        gravitationalConstant = 1.0  