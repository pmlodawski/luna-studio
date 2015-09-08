---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Particle.Algebra (
    Float, Vec3
  , Acceleration, Velocity, Position, TimeDiff, Array
  , zero
  , fma
  , addVec3, (.+)
  , mulVec3, (.*)
  , subVec3, (.-)
  , liftVec3
  , mulByScalar
  , divByScalar
  , magnitude
  , distanceVec3
  , normalize
  , negateVec3
) where

import qualified Data.Array.Accelerate as A

type Vec3 = (Float, Float, Float)

type Acceleration = Vec3
type Velocity = Vec3
type Position = Vec3
type TimeDiff = Float

type Array a = A.Vector a

distanceVec3 :: A.Exp Position -> A.Exp Position -> A.Exp Vec3
distanceVec3 p1 p2 = p1 `subVec3` p2

zero :: Vec3
zero = (0.0, 0.0, 0.0)

fma :: A.Acc (Array Vec3) ->
       A.Acc (Array Vec3) ->
       A.Acc (Array Vec3) ->
       A.Acc (Array Vec3)
fma a b c = (a .* b) .+ c

addVec3 :: A.Exp Vec3 -> A.Exp Vec3 -> A.Exp Vec3
addVec3 a b = A.lift2 (liftVec3 (+)) a b

(.+) :: A.Acc (Array Vec3) -> A.Acc (Array Vec3) -> A.Acc (Array Vec3)
v1 .+ v2 = A.zipWith addVec3 v1 v2

mulVec3 :: A.Exp Vec3 -> A.Exp Vec3 -> A.Exp Vec3
mulVec3 a b = A.lift2 (liftVec3 (*)) a b

(.*) :: A.Acc (Array Vec3) -> A.Acc (Array Vec3) -> A.Acc (Array Vec3)
v1 .* v2 = A.zipWith mulVec3 v1 v2

subVec3 :: A.Exp Vec3 -> A.Exp Vec3 -> A.Exp Vec3
subVec3 a b = A.lift2 (liftVec3 (-)) a b

(.-) :: A.Acc (Array Vec3) -> A.Acc (Array Vec3) -> A.Acc (Array Vec3)
v1 .- v2 = A.zipWith subVec3 v1 v2

liftVec3 :: (A.Exp a -> A.Exp a -> A.Exp a) ->
          (A.Exp a, A.Exp a, A.Exp a) ->
          (A.Exp a, A.Exp a, A.Exp a) ->
          (A.Exp a, A.Exp a, A.Exp a)
liftVec3 f (x1, x2, x3) (y1, y2, y3) = (x1 `f` y1, x2 `f` y2, x3 `f` y3)

magnitude :: A.Exp Vec3 -> A.Exp Float
magnitude p = sqrt $ x ** 2 + y ** 2 + z ** 2
  where
     (x,y,z) = A.unlift p :: (A.Exp Float, A.Exp Float, A.Exp Float)

liftScalar :: (A.Exp a -> A.Exp a -> A.Exp a) ->
              (A.Exp a, A.Exp a, A.Exp a) ->
               A.Exp a ->
              (A.Exp a, A.Exp a, A.Exp a)
liftScalar f (x1, x2, x3) y = (x1 `f` y, x2 `f` y, x3 `f` y)

mulByScalar :: A.Exp Vec3 -> A.Exp Float -> A.Exp Vec3
mulByScalar v x = A.lift (v1 * x, v2 * x, v3 * x)
  where
    (v1, v2, v3) = A.unlift v :: (A.Exp Float, A.Exp Float, A.Exp Float)

divByScalar :: A.Exp Vec3 -> A.Exp Float -> A.Exp Vec3
divByScalar v x = A.lift (v1 / x, v2 / x, v3 / x)
  where
    (v1, v2, v3) = A.unlift v :: (A.Exp Float, A.Exp Float, A.Exp Float)

normalize :: A.Exp Vec3 -> A.Exp Vec3
normalize vector = divByScalar vector $ magnitude vector

negateVec3 :: A.Exp Vec3 -> A.Exp Vec3
negateVec3 = A.lift1 neg
  where
    neg (x,y,z) = (-x, -y, -z) :: (A.Exp Float, A.Exp Float, A.Exp Float)
