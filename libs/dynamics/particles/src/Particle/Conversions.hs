{-# LANGUAGE RankNTypes #-}

module Particle.Conversions (
    toPositionVector
) where

import Control.Lens hiding (ix)
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO as AIO
import qualified Data.Vector.Storable as VectorS

import Particle
import Particle.Algebra

interleave3 :: A.Acc (Array Vec3) -> A.Acc (Array Float)
interleave3 v = A.generate sh interleave3'
  where
    sh = A.index1 (3 * A.size v)

    interleave3' ix =
      let i            = A.indexHead ix
          value        = v A.!! (i `div` 3)
          indexInTuple = i `mod` 3
          (x, y, z)    = A.unlift value :: (A.Exp Float, A.Exp Float, A.Exp Float)
      in
        indexInTuple A.==* 0 A.? (x, indexInTuple A.==* 1 A.? (y, z))

toPositionVector :: Backend -> ParticleSystem -> VectorS.Vector Float
toPositionVector backend ps = positionVector
  where
    ((), positionVector) = AIO.toVectors interleaved
    interleaved          = backend interleave3 $ ps ^. positions
