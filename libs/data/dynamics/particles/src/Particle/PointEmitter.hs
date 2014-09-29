---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Particle.PointEmitter (
    PointEmitter(..)
  , runPointEmitter
) where

import qualified Data.Array.Accelerate as A

import Particle
import Particle.Algebra

data PointEmitter = PointEmitter Position Int

runPointEmitter :: PointEmitter -> PipelinedSystem
runPointEmitter (PointEmitter emitterPosition spawnRate) ps = A.lift (timediff, newPositions, newVelocities, newAccelerations, newAges)
  where
    (timediff, pos, vel, acc, age) = A.unlift ps :: UnliftedPipelinedSystem

    newPositions = pos `fillWith` emitterPosition
    newVelocities = vel `fillWith` zero
    newAccelerations = acc `fillWith` zero
    newAges = age `fillWith` (0 :: Int)

    fillWith :: (A.Elt (A.Plain a), A.Lift A.Exp a, a ~ A.Plain a) => A.Acc (Array a) -> a -> A.Acc (Array a)
    fillWith array element = array A.++ spawnedArray element

    spawnedArray :: (A.Elt (A.Plain a), A.Lift A.Exp a) => a -> A.Acc (Array (A.Plain a))
    spawnedArray a         = A.fill (A.index1 $ A.constant spawnRate) $ A.lift a
