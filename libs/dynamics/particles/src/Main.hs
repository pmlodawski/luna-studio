---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Main where

import           Control.Lens               ((^.))
import           Data.AffineSpace           ((.-.))
import qualified Data.Array.Accelerate      as A
import           Data.Array.Accelerate.CUDA
import           Data.Default
import qualified Data.Thyme                 as Thyme

import Particle
import Particle.Conversions
import Particle.PerlinNoise
import Particle.PointAttractor
import Particle.PointEmitter

main :: IO ()
main = go def =<< Thyme.getCurrentTime

attractor = PointAttractor (4.0, 4.0, 0.0) 10
att = PointAttractor (-2.0, -6.0, 3.0) 15

go :: ParticleSystem -> Thyme.UTCTime -> IO ()
go sim time = do
  let sim'     = simulate pipeline t run1 sim
      pipeline = runPointAttractor attractor A.>-> runPointAttractor att A.>-> step
  print $ A.arraySize $ A.arrayShape $ sim' ^. positions
  currentTime <- Thyme.getCurrentTime
  print $ currentTime .-. time
  go sim' currentTime

t :: A.Scalar Float
t = run . A.unit . A.constant $ 0.1
