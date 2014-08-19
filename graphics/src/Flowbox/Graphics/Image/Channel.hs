---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Flowbox.Graphics.Image.Channel where

import           Data.Array.Accelerate (Shape, Elt, Exp)
import qualified Data.Array.Accelerate as A

import           Flowbox.Prelude hiding (use, zipWith)



data Channel a = Raw a
               | Acc (A.Acc a)
               deriving (Show)

type RawData2 a = A.Array A.DIM2 a
type RawData3 a = A.Array A.DIM3 a

type Backend ix a = A.Acc (A.Array ix a) -> A.Array ix a

type ChannelAcc ix a = Channel (A.Array ix a)
type Channel2 a = Channel (RawData2 a)
type Channel3 a = Channel (RawData3 a)

type Name = String

data Select = AllChannels
            | ChannelList [Name]

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- FIXME[PM] Fix these instances
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
instance Elt a => Eq (Channel a) where
    _ == _ = undefined

instance Elt a => Ord (Channel a) where
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

(!) :: (Shape ix, Elt e) => ChannelAcc ix e -> Exp ix -> Exp e
(!) channel = (A.!) $ accMatrix channel

(!!) :: (Shape ix, Elt e) => ChannelAcc ix e -> Exp Int -> Exp e
(!!) channel = (A.!!) $ accMatrix channel

the :: Elt e => Channel (A.Scalar e) -> Exp e
the channel = A.the $ accMatrix channel


-- Shape Information --

empty :: (Shape ix, Elt e) => ChannelAcc ix e -> Exp Bool
empty channel = A.null $ accMatrix channel

shape :: (Shape ix, Elt e) => ChannelAcc ix e -> Exp ix
shape channel = A.shape $ accMatrix channel

size :: (Shape ix, Elt e) => ChannelAcc ix e -> Exp Int
size channel = A.size $ accMatrix channel

shapeSize :: Shape ix => Exp ix -> Exp Int
shapeSize = A.shapeSize


-- = Extracting sub-arrays =

slice :: (A.Slice slix, Elt e) => ChannelAcc (A.FullShape slix) e -> Exp slix -> ChannelAcc (A.SliceShape slix) e
slice channel parts = Acc $ A.slice (accMatrix channel) parts


-- == Construction ==

-- = Introduction =

use :: A.Arrays a => Channel a -> Channel a
use chan = case chan of
    Raw m -> Acc $ A.use m
    Acc m -> Acc m

unit :: Elt e => Exp e -> Channel (A.Scalar e)
unit e = Acc $ A.unit e


-- = Initialisation =

generate :: (Shape ix, Elt e) => Exp ix -> (Exp ix -> Exp e) -> ChannelAcc ix e
generate sh f = Acc $ A.generate sh f

replicate :: (A.Slice slix, Elt e)
    => Exp slix -> ChannelAcc (A.SliceShape slix) e -> ChannelAcc (A.FullShape slix) e
replicate sh m = Acc $ A.replicate sh $ accMatrix m

fill :: (Shape ix, Elt e) => Exp ix -> Exp e -> ChannelAcc ix e
fill sh x = Acc $ A.fill sh x


-- = Enumeration =

enumFromN :: (Shape ix, Elt e, A.IsNum e) => Exp ix -> Exp e -> ChannelAcc ix e
enumFromN sh n = Acc $ A.enumFromN sh n

enumFromStepN :: (Shape ix, Elt e, A.IsNum e)
    => Exp ix -> Exp e -> Exp e -> ChannelAcc ix e
enumFromStepN sh n s = Acc $ A.enumFromStepN sh n s


-- = Concatenation =

(++) :: (A.Slice ix, Shape ix, Elt e) => ChannelAcc (ix A.:. Int) e -> ChannelAcc (ix A.:. Int) e -> ChannelAcc (ix A.:. Int) e
(++) chan1 chan2 = Acc $ accMatrix chan1 A.++ accMatrix chan2


-- == Modifying arrays ==

-- = Shape manipulation =

reshape :: (Shape ix, Shape ix', Elt e) => Exp ix -> ChannelAcc ix' e -> ChannelAcc ix e
reshape sh chan = Acc $ A.reshape sh (accMatrix chan)


-- = Specialised permutations =

transpose :: Elt e => Channel2 e -> Channel2 e
transpose chan = Acc $ A.transpose $ accMatrix chan


-- = Element-wise operations =

-- = Mapping =

map :: (Shape ix, Elt a, Elt b)
    => (Exp a -> Exp b) -> ChannelAcc ix a -> ChannelAcc ix b
map f channel = Acc $ A.map f (accMatrix channel)


-- = Zipping =

zipWith :: (Shape ix, Elt a, Elt b, Elt c)
    => (Exp a -> Exp b -> Exp c)
    -> ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
zipWith  f ch1 ch2 = Acc $ A.zipWith f (accMatrix ch1) (accMatrix ch2)

zipWith3 :: (Shape ix, Elt a, Elt b, Elt c, Elt d)
    => (Exp a -> Exp b -> Exp c -> Exp d)
    -> ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
zipWith3 f ch1 ch2 ch3 = Acc $ A.zipWith3 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3)

zipWith4 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e)
    -> ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
zipWith4 f ch1 ch2 ch3 ch4 = Acc $ A.zipWith4 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4)

zipWith5 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f)
    -> ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix f
zipWith5 f ch1 ch2 ch3 ch4 ch5 = Acc $ A.zipWith5 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5)

zipWith6 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g)
    -> ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix f
    -> ChannelAcc ix g
zipWith6 f ch1 ch2 ch3 ch4 ch5 ch6 = Acc $ A.zipWith6 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6)

zipWith7 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h)
    -> ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix f
    -> ChannelAcc ix g
    -> ChannelAcc ix h
zipWith7 f ch1 ch2 ch3 ch4 ch5 ch6 ch7 = Acc $ A.zipWith7 f (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7)

zipWith8 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i)
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

zipWith9 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i, Elt j)
    => (Exp a -> Exp b -> Exp c -> Exp d -> Exp e -> Exp f -> Exp g -> Exp h -> Exp i -> Exp j)
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


zip :: (Shape ix, Elt a, Elt b)
    => ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix (a, b)
zip  ch1 ch2 = Acc $ A.zip (accMatrix ch1) (accMatrix ch2)

zip3 :: (Shape ix, Elt a, Elt b, Elt c)
    => ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix (a, b, c)
zip3 ch1 ch2 ch3 = Acc $ A.zip3 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3)

zip4 :: (Shape ix, Elt a, Elt b, Elt c, Elt d)
    => ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix (a, b, c, d)
zip4 ch1 ch2 ch3 ch4 = Acc $ A.zip4 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4)

zip5 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e)
    => ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix (a, b, c, d, e)
zip5 ch1 ch2 ch3 ch4 ch5 = Acc $ A.zip5 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5)

zip6 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f)
    => ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix f
    -> ChannelAcc ix (a, b, c, d, e, f)
zip6 ch1 ch2 ch3 ch4 ch5 ch6 = Acc $ A.zip6 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6)

zip7 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g)
    => ChannelAcc ix a
    -> ChannelAcc ix b
    -> ChannelAcc ix c
    -> ChannelAcc ix d
    -> ChannelAcc ix e
    -> ChannelAcc ix f
    -> ChannelAcc ix g
    -> ChannelAcc ix (a, b, c, d, e, f, g)
zip7 ch1 ch2 ch3 ch4 ch5 ch6 ch7 = Acc $ A.zip7 (accMatrix ch1) (accMatrix ch2) (accMatrix ch3) (accMatrix ch4) (accMatrix ch5) (accMatrix ch6) (accMatrix ch7)

zip8 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h)
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

zip9 :: (Shape ix, Elt a, Elt b, Elt c, Elt d, Elt e, Elt f, Elt g, Elt h, Elt i)
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


-- == Unzipping ==

unzip ::  (Shape ix, Elt a)
    => ChannelAcc ix (a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a)
unzip chan = over each Acc $ A.unzip (accMatrix chan)

unzip3 :: (Shape ix, Elt a)
    => ChannelAcc ix (a, a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a)
unzip3 chan = over each Acc $ A.unzip3 (accMatrix chan)

unzip4 :: (Shape ix, Elt a)
    => ChannelAcc ix (a, a, a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a)
unzip4 chan = over each Acc $ A.unzip4 (accMatrix chan)

unzip5 :: (Shape ix, Elt a)
    => ChannelAcc ix (a, a, a, a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a)
unzip5 chan = over each Acc $ A.unzip5 (accMatrix chan)

unzip6 :: (Shape ix, Elt a)
    => ChannelAcc ix (a, a, a, a, a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a)
unzip6 chan = over each Acc $ A.unzip6 (accMatrix chan)

unzip7 :: (Shape ix, Elt a)
    => ChannelAcc ix (a, a, a, a, a, a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a)
unzip7 chan = over each Acc $ A.unzip7 (accMatrix chan)

unzip8 :: (Shape ix, Elt a)
    => ChannelAcc ix (a, a, a, a, a, a, a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a)
unzip8 chan = over each Acc $ A.unzip8 (accMatrix chan)

unzip9 :: (Shape ix, Elt a)
    => ChannelAcc ix (a, a, a, a, a, a, a, a, a)
    -> (ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a, ChannelAcc ix a)
unzip9 chan = over each Acc $ A.unzip9 (accMatrix chan)


-- == Folding ==

fold :: (Shape ix, Elt a) => (Exp a -> Exp a -> Exp a) -> Exp a -> ChannelAcc (ix A.:. Int) a -> ChannelAcc ix a
fold f acc chan = Acc $ A.fold f acc $ accMatrix chan

fold1 :: (Shape ix, Elt a) => (Exp a -> Exp a -> Exp a) -> ChannelAcc (ix A.:. Int) a -> ChannelAcc ix a
fold1 f chan = Acc $ A.fold1 f $ accMatrix chan

foldAll :: (Shape sh, Elt a) => (Exp a -> Exp a -> Exp a) -> Exp a -> ChannelAcc sh a -> Channel (A.Scalar a)
foldAll f acc chan = Acc $ A.foldAll f acc $ accMatrix chan

fold1All :: (Shape sh, Elt a) => (Exp a -> Exp a -> Exp a) -> ChannelAcc sh a -> Channel (A.Scalar a)
fold1All f chan = Acc $ A.fold1All f $ accMatrix chan

foldSeg :: (Shape ix, Elt a, Elt i, A.IsIntegral i) => (Exp a -> Exp a -> Exp a) -> Exp a -> ChannelAcc (ix A.:. Int) a -> A.Acc (A.Segments i) -> ChannelAcc (ix A.:. Int) a
foldSeg f acc chan segments = Acc $ A.foldSeg f acc (accMatrix chan) segments

fold1Seg :: (Shape ix, Elt a, Elt i, A.IsIntegral i) => (Exp a -> Exp a -> Exp a) -> ChannelAcc (ix A.:. Int) a -> A.Acc (A.Segments i) -> ChannelAcc (ix A.:. Int) a
fold1Seg f chan segments = Acc $ A.fold1Seg f (accMatrix chan) segments


-- = Stencil =

stencil :: (Shape ix, Elt a, Elt b, A.Stencil ix a stencil)
    => (stencil -> Exp b) -> A.Boundary a -> ChannelAcc ix a -> ChannelAcc ix b
stencil f b ch = Acc $ A.stencil f b (accMatrix ch)

stencil2 :: (Shape ix, Elt a, Elt b, Elt c, A.Stencil ix a stencil1, A.Stencil ix b stencil2)
    => (stencil1 -> stencil2 -> Exp c) -> A.Boundary a -> ChannelAcc ix a -> A.Boundary b -> ChannelAcc ix b -> ChannelAcc ix c
stencil2 f b1 ch1 b2 ch2 = Acc $ A.stencil2 f b1 (accMatrix ch1) b2 (accMatrix ch2)


-- == Operations ==

-- = Shape manipulation =

index3 :: (Elt i, A.Slice (A.Z A.:. i), A.Slice (A.Z A.:. i A.:. i))
    => Exp i -> Exp i -> Exp i -> Exp (A.Z A.:. i A.:. i A.:. i)
index3 i j k = A.lift (A.Z A.:. i A.:. j A.:. k)

lift2Dto3D :: (Elt a) => ChannelAcc A.DIM2 a -> ChannelAcc A.DIM3 a
lift2Dto3D channel = reshape (index3 a b 1) channel
    where (A.Z A.:. a A.:. b) = A.unlift $ shape channel
