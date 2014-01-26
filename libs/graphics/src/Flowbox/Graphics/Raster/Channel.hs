module Flowbox.Graphics.Raster.Channel where

import Flowbox.Prelude hiding (use)

import qualified Data.Array.Accelerate as A


type RawData a = A.Array A.DIM2 a

type Backend a = A.Acc (RawData a) -> (RawData a)

data Channel a = Raw (RawData a)
               | Acc (A.Acc (RawData a))
               deriving (Show)


map :: (A.Elt a, A.Elt b) => (A.Exp a -> A.Exp b) -> Channel a -> Channel b
map f chan = Acc $ A.map f ch
    where ch = case chan of
               Raw m -> A.use m
               Acc m -> m



use :: A.Elt a => Channel a -> Channel a
use chan = case chan of
    Raw m -> Acc $ A.use m
    Acc m -> Acc m


accMatrix :: A.Elt a => Channel a -> A.Acc(RawData a)
accMatrix chan = m where Acc m = use chan


compute :: Backend a -> Channel a -> Channel a
compute backend chan = Raw $ case chan of
    Raw m -> m
    Acc m -> backend m

--shape :: (A.Elt a, A.Shape sh) => Channel a -> sh
--shape :: Int
--shape ch = A.arrayShape $ (accMatrix ch)

--generate :: Int
--generate shape f = Acc $ A.generate shape f

stencil f b ch = Acc $ A.stencil f b (accMatrix ch)


zipWith  f ch1 ch2                             = Acc $ A.zipWith  f (accMatrix ch1) (accMatrix ch2)
zipWith3 f ch1 ch2 ch3                         = Acc $ A.zipWith3 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3)
zipWith4 f ch1 ch2 ch3 ch4                     = Acc $ A.zipWith4 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4)
zipWith5 f ch1 ch2 ch3 ch4 ch5                 = Acc $ A.zipWith5 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5)
zipWith6 f ch1 ch2 ch3 ch4 ch5 ch6             = Acc $ A.zipWith6 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6)
zipWith7 f ch1 ch2 ch3 ch4 ch5 ch6 ch7         = Acc $ A.zipWith7 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7)
zipWith8 f ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8     = Acc $ A.zipWith8 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7) (accMatrix ch8)
zipWith9 f ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8 ch9 = Acc $ A.zipWith9 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7) (accMatrix ch8) (accMatrix ch9)


zip  ch1 ch2                             = Acc $ A.zip  (accMatrix ch1) (accMatrix ch2)
zip3 ch1 ch2 ch3                         = Acc $ A.zip3 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3)
zip4 ch1 ch2 ch3 ch4                     = Acc $ A.zip4 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4)
zip5 ch1 ch2 ch3 ch4 ch5                 = Acc $ A.zip5 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5)
zip6 ch1 ch2 ch3 ch4 ch5 ch6             = Acc $ A.zip6 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6)
zip7 ch1 ch2 ch3 ch4 ch5 ch6 ch7         = Acc $ A.zip7 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7)
zip8 ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8     = Acc $ A.zip8 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7) (accMatrix ch8)
zip9 ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8 ch9 = Acc $ A.zip9 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7) (accMatrix ch8) (accMatrix ch9)


unzip  chan = over each Acc $ A.unzip  (accMatrix chan)
unzip3 chan = over each Acc $ A.unzip3 (accMatrix chan)
unzip4 chan = over each Acc $ A.unzip4 (accMatrix chan)
unzip5 chan = over each Acc $ A.unzip5 (accMatrix chan)
unzip6 chan = over each Acc $ A.unzip6 (accMatrix chan)
unzip7 chan = over each Acc $ A.unzip7 (accMatrix chan)
unzip8 chan = over each Acc $ A.unzip8 (accMatrix chan)
unzip9 chan = over each Acc $ A.unzip9 (accMatrix chan)
