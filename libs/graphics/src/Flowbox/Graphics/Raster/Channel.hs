---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Raster.Channel where

import qualified Data.Array.Accelerate as A

import Flowbox.Prelude hiding (use)


type RawData a = A.Array A.DIM2 a

type Backend a = A.Acc (RawData a) -> (RawData a)

data Channel a = Raw (RawData a)
               | Acc (A.Acc (RawData a))
               deriving (Show)

-- FIXME[PM] Fix these instances!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111111111111111
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
instance A.Elt a => Eq (Channel a) where
    _ == _ = undefined

instance A.Elt a => Ord (Channel a) where
    compare _ _ = undefined


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

--stencil :: (A.Stencil A.DIM2 a1 stencil, A.Elt a) => (stencil -> A.Exp a) -> A.Boundary a1 -> Channel a1 -> Channel a
stencil f b ch = Acc $ A.stencil f b (accMatrix ch)

zipWith :: (A.Elt a1, A.Elt b, A.Elt a) =>
    (A.Exp a1 -> A.Exp b -> A.Exp a)
    -> Channel a1 -> Channel b -> Channel a
zipWith  f ch1 ch2 = Acc $ A.zipWith  f (accMatrix ch1) (accMatrix ch2)

zipWith3 :: (A.Elt a1, A.Elt b, A.Elt c, A.Elt a) =>
    (A.Exp a1 -> A.Exp b -> A.Exp c -> A.Exp a)
    -> Channel a1 -> Channel b -> Channel c -> Channel a
zipWith3 f ch1 ch2 ch3 = Acc $ A.zipWith3 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3)

zipWith4 :: (A.Elt a1, A.Elt b, A.Elt c, A.Elt d, A.Elt a) =>
    (A.Exp a1 -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp a)
    -> Channel a1 -> Channel b -> Channel c -> Channel d -> Channel a
zipWith4 f ch1 ch2 ch3 ch4 = Acc $ A.zipWith4 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4)

zipWith5 :: (A.Elt a1, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt a) =>
    (A.Exp a1 -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp a)
    -> Channel a1 -> Channel b -> Channel c -> Channel d -> Channel e -> Channel a
zipWith5 f ch1 ch2 ch3 ch4 ch5                 = Acc $ A.zipWith5 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5)

zipWith6 :: (A.Elt a1, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt a) =>
    (A.Exp a1 -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp f -> A.Exp a)
    -> Channel a1 -> Channel b -> Channel c -> Channel d -> Channel e -> Channel f -> Channel a
zipWith6 f ch1 ch2 ch3 ch4 ch5 ch6             = Acc $ A.zipWith6 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6)

zipWith7 :: (A.Elt a1, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt a) =>
    (A.Exp a1 -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp f -> A.Exp g -> A.Exp a)
    -> Channel a1 -> Channel b -> Channel c -> Channel d -> Channel e -> Channel f -> Channel g -> Channel a
zipWith7 f ch1 ch2 ch3 ch4 ch5 ch6 ch7         = Acc $ A.zipWith7 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7)

zipWith8 :: (A.Elt a1, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt h, A.Elt a) =>
    (A.Exp a1 -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp f -> A.Exp g -> A.Exp h -> A.Exp a)
    -> Channel a1 -> Channel b -> Channel c -> Channel d -> Channel e -> Channel f -> Channel g -> Channel h -> Channel a
zipWith8 f ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8     = Acc $ A.zipWith8 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7) (accMatrix ch8)

zipWith9 :: (A.Elt a1, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt h, A.Elt i, A.Elt a) =>
    (A.Exp a1 -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp f -> A.Exp g -> A.Exp h -> A.Exp i -> A.Exp a)
    -> Channel a1 -> Channel b -> Channel c -> Channel d -> Channel e -> Channel f -> Channel g -> Channel h -> Channel i -> Channel a
zipWith9 f ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8 ch9 = Acc $ A.zipWith9 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7) (accMatrix ch8) (accMatrix ch9)


zip :: (A.Elt a, A.Elt b) =>
    Channel a -> Channel b -> Channel (a, b)
zip  ch1 ch2 = Acc $ A.zip  (accMatrix ch1) (accMatrix ch2)

zip3 :: (A.Elt a, A.Elt b, A.Elt c) =>
    Channel a -> Channel b -> Channel c -> Channel (a, b, c)
zip3 ch1 ch2 ch3 = Acc $ A.zip3 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3)

zip4 :: (A.Elt a, A.Elt b, A.Elt c, A.Elt d) =>
    Channel a -> Channel b -> Channel c -> Channel d -> Channel (a, b, c, d)
zip4 ch1 ch2 ch3 ch4                     = Acc $ A.zip4 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4)

zip5 :: (A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e) =>
    Channel a -> Channel b -> Channel c -> Channel d -> Channel e -> Channel (a, b, c, d, e)
zip5 ch1 ch2 ch3 ch4 ch5                 = Acc $ A.zip5 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5)

zip6 :: (A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f) =>
    Channel a -> Channel b -> Channel c -> Channel d -> Channel e -> Channel f -> Channel (a, b, c, d, e, f)
zip6 ch1 ch2 ch3 ch4 ch5 ch6             = Acc $ A.zip6 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6)

zip7 ::  (A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g) =>
    Channel a -> Channel b -> Channel c -> Channel d -> Channel e -> Channel f -> Channel g -> Channel (a, b, c, d, e, f, g)
zip7 ch1 ch2 ch3 ch4 ch5 ch6 ch7         = Acc $ A.zip7 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7)

zip8 :: (A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt h) =>
    Channel a -> Channel b -> Channel c -> Channel d -> Channel e -> Channel f -> Channel g -> Channel h -> Channel (a, b, c, d, e, f, g, h)
zip8 ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8     = Acc $ A.zip8 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7) (accMatrix ch8)

zip9 :: (A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt h, A.Elt i) =>
    Channel a -> Channel b -> Channel c -> Channel d -> Channel e -> Channel f -> Channel g -> Channel h -> Channel i -> Channel (a, b, c, d, e, f, g, h, i)
zip9 ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8 ch9 = Acc $ A.zip9 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7) (accMatrix ch8) (accMatrix ch9)


unzip ::  A.Elt a => Channel (a, a)
    -> (Channel a, Channel a)
unzip  chan = over each Acc $ A.unzip  (accMatrix chan)

unzip3 :: A.Elt a => Channel (a, a, a)
    -> (Channel a, Channel a, Channel a)
unzip3 chan = over each Acc $ A.unzip3 (accMatrix chan)

unzip4 ::  A.Elt a => Channel (a, a, a, a)
    -> (Channel a, Channel a, Channel a, Channel a)
unzip4 chan = over each Acc $ A.unzip4 (accMatrix chan)

unzip5 ::  A.Elt a => Channel (a, a, a, a, a)
    -> (Channel a, Channel a, Channel a, Channel a, Channel a)
unzip5 chan = over each Acc $ A.unzip5 (accMatrix chan)

unzip6 :: A.Elt a => Channel (a, a, a, a, a, a)
    -> (Channel a, Channel a, Channel a, Channel a, Channel a, Channel a)
unzip6 chan = over each Acc $ A.unzip6 (accMatrix chan)

unzip7 :: A.Elt a => Channel (a, a, a, a, a, a, a)
    -> (Channel a, Channel a, Channel a, Channel a, Channel a, Channel a, Channel a)
unzip7 chan = over each Acc $ A.unzip7 (accMatrix chan)

unzip8 ::  A.Elt a => Channel (a, a, a, a, a, a, a, a)
    -> (Channel a, Channel a, Channel a, Channel a, Channel a, Channel a, Channel a, Channel a)
unzip8 chan = over each Acc $ A.unzip8 (accMatrix chan)

unzip9 :: A.Elt a => Channel (a, a, a, a, a, a, a, a, a)
    -> (Channel a, Channel a, Channel a, Channel a, Channel a, Channel a, Channel a, Channel a, Channel a)
unzip9 chan = over each Acc $ A.unzip9 (accMatrix chan)
