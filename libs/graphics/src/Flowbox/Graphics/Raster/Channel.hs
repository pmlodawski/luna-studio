module Flowbox.Graphics.Raster.Channel where

import Flowbox.Prelude

import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.CUDA    as CUDA

data Channel a = Raw (A.Array A.DIM2 a)
               | Acc (A.Acc (A.Array A.DIM2 a))
               deriving (Show)


map f chan = Acc $ A.map f ch
    where ch = case chan of
               Raw m -> A.use m
               Acc m -> m 


use chan = case chan of 
    Raw m -> A.use m
    Acc m -> m


compute chan = Raw $ case chan of 
    Raw m -> m
    Acc m -> CUDA.run m