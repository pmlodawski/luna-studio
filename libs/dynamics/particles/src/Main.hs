module Main where

import Control.Lens ((^.))
import Data.Default
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.CUDA
import qualified Data.Thyme as Thyme
import Data.AffineSpace ((.-.))

import Particle
import Particle.PointEmitter
import Particle.PointAttractor
import Particle.Conversions
import Particle.PerlinNoise

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