module Flowbox.Graphics.Raster.Channel where

import Flowbox.Prelude

import qualified Data.Array.Accelerate         as A

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


compute backend chan = Raw $ case chan of 
    Raw m -> m
    Acc m -> backend m