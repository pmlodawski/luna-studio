---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Flowbox.Graphics.Image.Channel where

import qualified Data.Array.Accelerate as A

import Flowbox.Prelude hiding (use, zipWith)


data Channel a = Raw (a)
               | Acc (A.Acc (a))
               deriving (Show)

type RawData2 a = A.Array A.DIM2 a
type RawData3 a = A.Array A.DIM3 a

type Backend ix a = A.Acc (A.Array ix a) -> (A.Array ix a)

type ChannelAcc ix a = Channel (A.Array ix a)
type Channel2 a = Channel (RawData2 a)
type Channel3 a = Channel (RawData3 a)

type Name = String

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- FIXME[PM] Fix these instances
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
instance A.Elt a => Eq (Channel a) where
    _ == _ = undefined

instance A.Elt a => Ord (Channel a) where
    compare _ _ = undefined

accMatrix :: A.Arrays a => Channel a -> A.Acc a
accMatrix chan = case chan of
    Raw m -> A.use m
    Acc m -> m

compute :: Backend ix e -> ChannelAcc ix e -> ChannelAcc ix e
compute backend chan = Raw $ case chan of
    Raw m -> m
    Acc m -> backend m

-- == Accessors ==

-- = Indexing =

(!) :: (A.Shape ix, A.Elt e) => ChannelAcc ix e -> A.Exp ix -> A.Exp e
(!) channel = (A.!) $ accMatrix channel

(!!) :: (A.Shape ix, A.Elt e) => ChannelAcc ix e -> A.Exp Int -> A.Exp e
(!!) channel = (A.!!) $ accMatrix channel

-- Shape Information --

empty :: (A.Shape ix, A.Elt e) => ChannelAcc ix e -> A.Exp Bool
empty channel = A.null $ accMatrix channel

shape :: (A.Shape ix, A.Elt e) => ChannelAcc ix e -> A.Exp ix
shape channel = A.shape $ accMatrix channel

size :: (A.Shape ix, A.Elt e) => ChannelAcc ix e -> A.Exp Int
size channel = A.size $ accMatrix channel

shapeSize :: A.Shape ix => A.Exp ix -> A.Exp Int
shapeSize = A.shapeSize

-- = Extracting sub-arrays =

slice :: (A.Slice slix, A.Elt e) => ChannelAcc (A.FullShape slix) e -> A.Exp slix -> ChannelAcc (A.SliceShape slix) e
slice channel parts = Acc $ A.slice (accMatrix channel) parts

-- == Construction ==

-- = Introduction =

use :: A.Arrays a => Channel a -> Channel a
use chan = case chan of
    Raw m -> Acc $ A.use m
    Acc m -> Acc m

unit :: A.Elt e => A.Exp e -> Channel (A.Scalar e)
unit e = Acc $ A.unit e

-- = Initialisation =

generate :: (A.Shape ix, A.Elt e) => A.Exp ix -> (A.Exp ix -> A.Exp e) -> ChannelAcc ix e
generate sh f = Acc $ A.generate sh f

replicate :: (A.Slice slix, A.Elt e)
    => A.Exp slix -> ChannelAcc (A.SliceShape slix) e -> ChannelAcc (A.FullShape slix) e
replicate sh m = Acc $ A.replicate sh $ accMatrix m

fill :: (A.Shape ix, A.Elt e) => A.Exp ix -> A.Exp e -> ChannelAcc ix e
fill sh x = Acc $ A.fill sh x

-- = Enumeration =

enumFromN :: (A.Shape ix, A.Elt e, A.IsNum e) => A.Exp ix -> A.Exp e -> ChannelAcc ix e
enumFromN sh n = Acc $ A.enumFromN sh n

enumFromStepN :: (A.Shape ix, A.Elt e, A.IsNum e)
    => A.Exp ix -> A.Exp e -> A.Exp e -> ChannelAcc ix e
enumFromStepN sh n s = Acc $ A.enumFromStepN sh n s

-- = Concatenation =

(++) :: (A.Slice ix, A.Shape ix, A.Elt e) => ChannelAcc (ix A.:. Int) e -> ChannelAcc (ix A.:. Int) e -> ChannelAcc (ix A.:. Int) e
(++) chan1 chan2 = Acc $ (accMatrix chan1) A.++ (accMatrix chan2)


-- == Modifying arrays ==

-- = Shape manipulation =

reshape :: (A.Shape ix, A.Shape ix', A.Elt e) => A.Exp ix -> ChannelAcc ix' e -> ChannelAcc ix e
reshape sh chan = Acc $ A.reshape sh (accMatrix chan)

-- = Specialised permutations =

transpose :: A.Elt e => Channel2 e -> Channel2 e
transpose chan = Acc $ accMatrix chan


-- = Element-wise operations =

-- = Mapping =

map :: (A.Shape ix, A.Elt a, A.Elt b)
    => (A.Exp a -> A.Exp b) -> ChannelAcc ix a -> ChannelAcc ix b
map f channel = Acc $ A.map f (accMatrix channel)

-- = Zipping =

zipWith :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c)
    => (A.Exp a -> A.Exp b -> A.Exp c)
    -> ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
zipWith  f ch1 ch2 = Acc $ A.zipWith f (accMatrix ch1) (accMatrix ch2)

zipWith3 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d)
    => (A.Exp a -> A.Exp b -> A.Exp c -> A.Exp d)
    -> ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
zipWith3 f ch1 ch2 ch3 = Acc $ A.zipWith3 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3)

zipWith4 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e)
    => (A.Exp a -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e)
    -> ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
zipWith4 f ch1 ch2 ch3 ch4 = Acc $ A.zipWith4 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4)

zipWith5 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f)
    => (A.Exp a -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp f)
    -> ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix f
zipWith5 f ch1 ch2 ch3 ch4 ch5 = Acc $ A.zipWith5 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5)

zipWith6 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g)
    => (A.Exp a -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp f -> A.Exp g)
    -> ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix f
    -> ChannelAcc ix g
zipWith6 f ch1 ch2 ch3 ch4 ch5 ch6 = Acc $ A.zipWith6 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6)

zipWith7 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt h)
    => (A.Exp a -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp f -> A.Exp g -> A.Exp h)
    -> ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix f
    -> ChannelAcc ix g
    -> ChannelAcc ix h
zipWith7 f ch1 ch2 ch3 ch4 ch5 ch6 ch7 = Acc $ A.zipWith7 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7)

zipWith8 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt h, A.Elt i)
    => (A.Exp a -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp f -> A.Exp g -> A.Exp h -> A.Exp i)
    -> ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix f
    -> ChannelAcc ix g
    -> ChannelAcc ix h
    -> ChannelAcc ix i
zipWith8 f ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8 = Acc $ A.zipWith8 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7) (accMatrix ch8)

zipWith9 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt h, A.Elt i, A.Elt j)
    => (A.Exp a -> A.Exp b -> A.Exp c -> A.Exp d -> A.Exp e -> A.Exp f -> A.Exp g -> A.Exp h -> A.Exp i -> A.Exp j)
    -> ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix f
    -> ChannelAcc ix g
    -> ChannelAcc ix h
    -> ChannelAcc ix i
    -> ChannelAcc ix j
zipWith9 f ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8 ch9 = Acc $ A.zipWith9 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7) (accMatrix ch8) (accMatrix ch9)


zip :: (A.Shape ix, A.Elt a, A.Elt b)
    => ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix (a, b)
zip  ch1 ch2 = Acc $ A.zip (accMatrix ch1) (accMatrix ch2)

zip3 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c)
    => ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix (a, b, c)
zip3 ch1 ch2 ch3 = Acc $ A.zip3 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3)

zip4 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d)
    => ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix (a, b, c, d)
zip4 ch1 ch2 ch3 ch4 = Acc $ A.zip4 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4)

zip5 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e)
    => ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix (a, b, c, d, e)
zip5 ch1 ch2 ch3 ch4 ch5 = Acc $ A.zip5 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5)

zip6 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f)
    => ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix f
    -> ChannelAcc ix (a, b, c, d, e, f)
zip6 ch1 ch2 ch3 ch4 ch5 ch6 = Acc $ A.zip6 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6)

zip7 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g)
    => ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix f
    -> ChannelAcc ix g
    -> ChannelAcc ix (a, b, c, d, e, f, g)
zip7 ch1 ch2 ch3 ch4 ch5 ch6 ch7 = Acc $ A.zip7 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7)

zip8 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt h)
    => ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix f
    -> ChannelAcc ix g
    -> ChannelAcc ix h
    -> ChannelAcc ix (a, b, c, d, e, f, g, h)
zip8 ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8 = Acc $ A.zip8 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7) (accMatrix ch8)

zip9 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Elt d, A.Elt e, A.Elt f, A.Elt g, A.Elt h, A.Elt i)
    => ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix f
    -> ChannelAcc ix g
    -> ChannelAcc ix h
    -> ChannelAcc ix i
    -> ChannelAcc ix (a, b, c, d, e, f, g, h, i)
zip9 ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8 ch9 = Acc $ A.zip9 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7) (accMatrix ch8) (accMatrix ch9)


unzip ::  (A.Shape ix, A.Elt a)
    => ChannelAcc ix (a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a)
unzip chan = over each Acc $ A.unzip (accMatrix chan)
--unzip chan = over each Acc $ A.unzip  (accMatrix chan)

unzip3 :: (A.Shape ix, A.Elt a)
    => ChannelAcc ix (a, a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a)
unzip3 chan = over each Acc $ A.unzip3 (accMatrix chan)

unzip4 :: (A.Shape ix, A.Elt a)
    => ChannelAcc ix (a, a, a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a)
unzip4 chan = over each Acc $ A.unzip4 (accMatrix chan)

unzip5 :: (A.Shape ix, A.Elt a)
    => ChannelAcc ix (a, a, a, a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a)
unzip5 chan = over each Acc $ A.unzip5 (accMatrix chan)

unzip6 :: (A.Shape ix, A.Elt a)
    => ChannelAcc ix (a, a, a, a, a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a)
unzip6 chan = over each Acc $ A.unzip6 (accMatrix chan)

unzip7 :: (A.Shape ix, A.Elt a)
    => ChannelAcc ix (a, a, a, a, a, a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a)
unzip7 chan = over each Acc $ A.unzip7 (accMatrix chan)

unzip8 :: (A.Shape ix, A.Elt a)
    => ChannelAcc ix (a, a, a, a, a, a, a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a)
unzip8 chan = over each Acc $ A.unzip8 (accMatrix chan)

unzip9 :: (A.Shape ix, A.Elt a)
    => ChannelAcc ix (a, a, a, a, a, a, a, a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a)
unzip9 chan = over each Acc $ A.unzip9 (accMatrix chan)


-- == Stencil ==

stencil :: (A.Shape ix, A.Elt a, A.Elt b, A.Stencil ix a stencil)
    => (stencil -> A.Exp b) -> A.Boundary a -> ChannelAcc ix a -> ChannelAcc ix b
stencil f b ch = Acc $ A.stencil f b (accMatrix ch)

stencil2 :: (A.Shape ix, A.Elt a, A.Elt b, A.Elt c, A.Stencil ix a stencil1, A.Stencil ix b stencil2)
    => (stencil1 -> stencil2 -> A.Exp c) -> A.Boundary a -> ChannelAcc ix a -> A.Boundary b -> ChannelAcc ix b -> ChannelAcc ix c
stencil2 f b1 ch1 b2 ch2 = Acc $ A.stencil2 f b1 (accMatrix ch1) b2 (accMatrix ch2)


-- == Operations ==

-- = Shape manipulation =

index3 :: (A.Elt i, A.Slice (A.Z A.:. i), A.Slice (A.Z A.:. i A.:. i))
    => A.Exp i -> A.Exp i -> A.Exp i -> A.Exp (A.Z A.:. i A.:. i A.:. i)
index3 i j k = A.lift (A.Z A.:. i A.:. j A.:. k)
