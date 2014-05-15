{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Particle (
    ParticleSystem()
  , positions
  , velocities
  , accelerations
  , ages
  , simulate
  , create
  , create'
  , create''
  , listArray
  , initialArrays
  , PipelinedSystem
  , UnliftedPipelinedSystem
  , simToPipelined
  , pipelinedToSim
  , step
  , Backend
) where

import qualified Data.Array.Accelerate as A
import qualified Data.Maybe as Maybe
import Data.Default (Default, def)
import Control.Applicative ((<$>), (<*>))
import Control.Lens

import Particle.Algebra

data ParticleSystem = ParticleSystem {
    _positions     :: Array Position
  , _velocities    :: Array Velocity
  , _accelerations :: Array Acceleration
  , _ages          :: Array Int
}

makeLenses ''ParticleSystem

type PipelinedSystem = A.Acc (A.Scalar TimeDiff, Array Position, Array Velocity, Array Acceleration, Array Int)
                       -> A.Acc (A.Scalar TimeDiff, Array Position, Array Velocity, Array Acceleration, Array Int)

type UnliftedPipelinedSystem = (A.Acc (A.Scalar TimeDiff), A.Acc (Array Position), A.Acc (Array Velocity),
                                A.Acc (Array Acceleration), A.Acc (Array Int))

type Backend = forall a b. (A.Arrays a, A.Arrays b) => (A.Acc a -> A.Acc b) -> a -> b

-- FIXME [MM]: qualified wszystko oprocz lens, monoid, default, control.monad,
--             libs.utils.flowbox.prelude

instance Default ParticleSystem where
  def = ParticleSystem { _positions        = zeroArray
                       , _velocities       = zeroArray
                       , _accelerations    = zeroArray
                       , _ages             = replicate particlesCount 0 ^. listArray
                       }
    where
      zeroArray      = replicate particlesCount zero ^. listArray
      particlesCount = 5000000

--stepSystem :: TimeDiff -> ParticleSystem -> ParticleSystem
--stepSystem seconds ps = ps {
--      _positions      = CUDA.run updatedPositions
--    , _velocities     = CUDA.run updatedVelocities
--    , _accelerations  = CUDA.run $ A.fill particlesArrayShapeExp (A.lift zero :: A.Exp Acceleration)
--    , _ages           = CUDA.run $ A.map (+1) (A.use $ ps ^. ages)
--  }
--  where
--    updatedVelocities = fma timeArray (A.use acc') (A.use vel')
--    updatedPositions  = fma timeArray updatedVelocities (A.use pos')

--    pos' = ps ^. positions
--    vel' = ps ^. velocities
--    acc' = ps ^. accelerations

--    particlesArrayShapeExp = A.lift $ A.arrayShape $ ps ^. positions

--    timeArray = A.fill particlesArrayShapeExp $ A.lift seconds'
--      where
--        seconds' = (\a -> (a,a,a)) seconds

--simulate :: A.Scalar TimeDiff -> ParticleSystem -> ParticleSystem
--simulate time ps = toPS (CUDA.run1 step fromPS)
--  where toPS :: (A.Scalar TimeDiff, Array Position, Array Velocity, Array Acceleration, Array Int) ->
--                ParticleSystem
--        toPS (_, p, v, a, ages') = ps & positions .~ p & velocities .~ v & accelerations .~ a &
--          ages .~ ages'

--        fromPS :: (A.Scalar TimeDiff, Array Position, Array Velocity, Array Acceleration, Array Int)
--        fromPS = (time, ps ^. positions, ps ^. velocities, ps ^. accelerations, ps ^. ages)

simToPipelined :: A.Scalar TimeDiff -> ParticleSystem -> (A.Scalar TimeDiff, Array Position, Array Velocity, Array Acceleration, Array Int)
simToPipelined td ps = (td, ps ^. positions, ps ^. velocities, ps ^. accelerations, ps ^. ages)

pipelinedToSim :: (A.Scalar TimeDiff, Array Position, Array Velocity, Array Acceleration, Array Int) -> ParticleSystem
pipelinedToSim (_, pos, vel, acc, age) = ParticleSystem {
      _positions = pos
    , _velocities = vel
    , _accelerations = acc
    , _ages = age
  }

simulate :: PipelinedSystem -> A.Scalar TimeDiff -> Backend -> ParticleSystem -> ParticleSystem
simulate pipe time backend ps = pipelinedToSim $ backend pipe $ simToPipelined time ps
 
step :: A.Acc (A.Scalar TimeDiff, Array Position, Array Velocity, Array Acceleration, Array Int) ->
        A.Acc (A.Scalar TimeDiff, Array Position, Array Velocity, Array Acceleration, Array Int)
step arg = A.lift (time, updatedPositions, updatedVelocities, updatedAccelerations, updatedAges)
  where (time, pos, vel, acc, age) = A.unlift arg

        timeExp = A.the time
        time'   = A.lift (timeExp, timeExp, timeExp)

        updatedVelocities    = vel .+ A.map (`mulVec3` time') acc
        updatedPositions     = pos .+ A.map (`mulVec3` time') updatedVelocities
        updatedAccelerations = A.fill particlesShape (A.the . A.unit . A.constant $ zero)
        updatedAges          = A.map (+1) age

        particlesShape = A.shape pos

listArray :: (A.Elt a) => Iso' [a] (Array a)
listArray = iso list2array array2list
  where
    list2array l = A.fromList (A.Z A.:. length l) l
    array2list   = A.toList

data PSOptions = PSOptions {
    _initialPositions     :: Maybe (Array Position)
  , _initialVelocities    :: Maybe (Array Velocity)
  , _initialAccelerations :: Maybe (Array Acceleration)
}

makeLenses ''PSOptions

instance Default PSOptions where
  def = PSOptions Nothing Nothing Nothing

initialArrays :: Traversal' PSOptions (Maybe (Array Vec3))
initialArrays f (PSOptions pos' vel' acc') =
  PSOptions <$> f pos' <*> f vel' <*> f acc'

create :: ParticleSystem
create = create' def

create'' :: (PSOptions -> PSOptions) -> ParticleSystem
create'' f = create' (f def)

create' :: PSOptions -> ParticleSystem
create' opts
  | anyArrayProvided = def
    & positions     .~ positions' ^. from listArray . to (take shortestArrayLength) . listArray
    & velocities    .~ velocities' ^. from listArray . to (take shortestArrayLength) . listArray
    & accelerations .~ accelerations' ^. from listArray . to (take shortestArrayLength) . listArray
    & ages          .~ replicate shortestArrayLength 0 ^. listArray
  | otherwise        = def
  where
    shortestArrayLength =
      fromIntegral $ minimum $ map arraySize' $ Maybe.catMaybes $ opts ^.. initialArrays

    arraySize' = A.arraySize . A.arrayShape

    anyArrayProvided = anyOf initialArrays (isn't _Nothing) opts

    positions'     = Maybe.fromMaybe (def ^. positions) (opts ^. initialPositions)
    velocities'    = Maybe.fromMaybe (def ^. velocities) (opts ^. initialVelocities)
    accelerations' = Maybe.fromMaybe (def ^. accelerations) (opts ^. initialAccelerations)