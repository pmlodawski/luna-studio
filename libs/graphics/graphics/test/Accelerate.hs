---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude as P

import Data.Array.Accelerate as A
import Data.Array.Accelerate as M
import Data.Array.Accelerate.IO
import Data.Array.Accelerate.CUDA

import Linear.Accelerate
import Linear.V2

type Matrix2 a = Acc (Array DIM2 a)
type Grid a = V2 a
type Point2 a = V2 a
type EDIM2 = Z :. Exp Int :. Exp Int

-- --== Helper functions ==--

clamp :: (Elt t, IsScalar t) => Exp t -> Exp t -> Exp t -> Exp t
clamp low high val = (val A.>* high) A.? (high, (val A.<* low) A.? (low, val))

boundedIndex :: Elt e => Boundary (Exp e) -> Matrix2 e -> Exp DIM2 -> Exp e
boundedIndex b mat i = case b of
    Clamp      -> mat ! index2 ((y `min` h1) `max` 0) ((x `min` w1) `max` 0)
    Mirror     -> mat ! index2 (abs $ -abs (y `mod` (2 * height) - h1) + h1) (abs $ -abs (x `mod` (2 * width) - w1) + w1)
    Wrap       -> mat ! index2 (y `mod` height) (x `mod` width)
    Constant a -> (x >=* width ||* y >=* height ||* x <* 0 ||* y <* 0) ? (a, mat ! i)
    where Z :. height :. width = unlift $ shape mat
          Z :. y      :. x     = unlift i
          h1 = height - 1
          w1 = width - 1

testLoadRGBA :: (Elt a, IsFloating a) => FilePath -> IO (Matrix2 a, Matrix2 a, Matrix2 a, Matrix2 a)
testLoadRGBA filename = do
    file <- readImageFromBMP filename
    case file of
        Right mat -> return $ M.unzip4 $ M.map (convert . unpackRGBA32) $ A.use mat
        Left e -> error $ "Unable to load file: " P.++ show e
    where convert t = let (r, g, b, a) = A.unlift t :: (Exp A.Word8, Exp A.Word8, Exp A.Word8, Exp A.Word8)
                      in A.lift (A.fromIntegral r / 255, A.fromIntegral g / 255, A.fromIntegral b / 255, A.fromIntegral a / 255)

testSaveRGBA :: (Elt a, IsFloating a) => FilePath -> Matrix2 a -> Matrix2 a -> Matrix2 a -> Matrix2 a -> IO ()
testSaveRGBA filename r g b a = writeImageToBMP filename $ run $ M.map packRGBA32 $ zip4 (conv r) (conv g) (conv b) (conv a)
    where conv = id >-> M.map (A.truncate . (* 255.0) . clamp 0 1)

-- --== Generator API ==--

newtype Generator a b = Generator {
    runGenerator :: Point2 a -> b
}

type DiscreteGenerator = Generator (Exp Int)

rasterizer :: Elt e => Grid (Exp Int) -> DiscreteGenerator (Exp e) -> Matrix2 e
rasterizer (V2 h w) (Generator gen) = generate (A.index2 h w) wrapper
    where wrapper (A.unlift -> Z :. y :. x :: EDIM2) = gen (V2 x y)

fromMatrix :: Elt e => Boundary (Exp e) -> Matrix2 e -> DiscreteGenerator (Exp e)
fromMatrix b mat = Generator $ \(V2 x y) -> boundedIndex b mat $ A.index2 y x

monosampler :: (Elt a, IsNum a) => Generator (Exp a) e -> DiscreteGenerator e
monosampler (Generator gen) = Generator $ \pixel -> gen (fmap A.fromIntegral pixel)

interpolator :: (Elt e, IsNum e, IsFloating e) => Matrix2 e -> DiscreteGenerator (Exp e) -> Generator (Exp e) (Exp e)
interpolator filter (Generator gen) = Generator $ \(V2 x y) ->
    let ksh = shape filter
        Z :. h :. w = A.unlift ksh
        dxs = generate ksh $ \(A.unlift -> Z :. y :. x :: EDIM2) -> A.fromIntegral (x - w `div` 2)
        dys = generate ksh $ \(A.unlift -> Z :. y :. x :: EDIM2) -> A.fromIntegral (y - h `div` 2)
        offsets = flatten $ M.zip3 filter dxs dys

        lift :: Elt x => Exp x -> Exp (x, x)
        lift a = A.lift (a, a)

        calc (A.unlift -> (valSum, weightSum)) (A.unlift -> (weight, dx, dy)) = A.lift (valSum + value * weight, weightSum + weight)
            where value = gen $ V2 (A.floor $ x + dx) (A.floor $ y + dy)
    in A.uncurry (/) $ sfoldl calc (lift 0) A.index0 offsets

-- == Filters ==

data Filter a = Filter { window :: Exp a
                       , apply :: Exp a -> Exp a
                       }

box :: (Elt a, IsFloating a) => Filter a
box = Filter 0.5 $ \t -> A.cond (t >* -0.5 &&* t <=*  0.5) 1.0 0.0

toMatrix :: (Elt a, IsFloating a) => Exp Int -> Filter a -> Matrix2 a
toMatrix size filter = M.generate (A.index2 size size) $ \(A.unlift -> Z :. y :. x :: EDIM2) ->
    let scale = 2 * window filter / A.fromIntegral size
        vx = apply filter $ A.fromIntegral (x - size `div` 2) * scale
        vy = apply filter $ A.fromIntegral (y - size `div` 2) * scale
    in vx * vy

-- == Bug demonstration ==

main = do
    (r, g, b, a) <- testLoadRGBA "lena.bmp"
    let process x = rasterizer (V2 1024 1024) $ monosampler $ interpolator (toMatrix 2 box) $ fromMatrix (Clamp :: Boundary (Exp Float)) x
    testSaveRGBA "out.bmp" (process r) (process g) (process b) (process a)

{- Traceback: 

1.61:gc: initialise default context
1.78:gc: initialise context #0x00007fa62800da30
1.78:gc: push context: #0x00007fa62800da30
1.79:gc: initialise CUDA state
1.79:gc: initialise memory table
1.79:cc: initialise kernel table
1.79:cc: deleting persistent cache
1.79:gc: lookup/not found: Array #858
1.79:gc: malloc/new
1.79:gc: insert: Array #858
1.79:gc: useArrayAsync/malloc: 1 MB @ 1.886 GB/s, gpu: 517.792 µs, cpu: 0.000 Ts
1.80:cc: (3.0,"\\t\129\208s.\207\197\167F\211P2\b5\139")
#include <accelerate_cuda.h>
extern "C" __global__ void map(const Int64 shIn0_1, const Int64 shIn0_0, const Word32* __restrict__ arrIn0_0, const Int64 shOut_1, const Int64 shOut_0, float* __restrict__ arrOut_3, float* __restrict__ arrOut_2, float* __restrict__ arrOut_1, float* __restrict__ arrOut_0)
{
const int shapeSize = shOut_1 * shOut_0;
const int gridSize = blockDim.x * gridDim.x;
int ix;

for (ix = blockDim.x * blockIdx.x + threadIdx.x; ix < shapeSize; ix += gridSize) {
const Word32 x0 = arrIn0_0[ix];
const Word32 v0 = (Word32) 255;
const Word8 v1 = (Word8) (v0 & x0);
const Word8 v2 = (Word8) (v0 & idiv(x0, (Word32) 256));
const Word8 v3 = (Word8) (v0 & idiv(x0, (Word32) 65536));
const Word8 v4 = (Word8) (v0 & idiv(x0, (Word32) 16777216));

arrOut_3[ix] = (float) v1 / 255.0f;
arrOut_2[ix] = (float) v2 / 255.0f;
arrOut_1[ix] = (float) v3 / 255.0f;
arrOut_0[ix] = (float) v4 / 255.0f;
}
}


1.80:cc: (3.0,"\152\214k\170\130f(\135\159\175a\200}B`\164")
#include <accelerate_cuda.h>
extern "C" __global__ void generate(const Int64 shIn0_1, const Int64 shIn0_0, const float* __restrict__ arrIn0_3, const float* __restrict__ arrIn0_2, const float* __restrict__ arrIn0_1, const float* __restrict__ arrIn0_0, const Int64 shOut_1, const Int64 shOut_0, float* __restrict__ arrOut_0)
{
const int shapeSize = shOut_1 * shOut_0;
const int gridSize = blockDim.x * gridDim.x;
int ix;

for (ix = blockDim.x * blockIdx.x + threadIdx.x; ix < shapeSize; ix += gridSize) {
const Int64 tmp_0 = ix;
const Int64 tmp_1 = tmp_0 / shOut_0;
const Int64 sh1 = tmp_1 % shOut_1;
const Int64 sh0 = tmp_0 % shOut_0;
float lv00 = 0.0f;
float lv01 = 0.0f;
Int64 lv02 = (Int64) 0;
Word8 lv10;

lv10 = lv02 < v3 * v3;

const Int64 v3 = (Int64) 2;

while (lv10) {
Int64 lv22;
float lv21;
float lv20;
const Int64 v4 = (Int64) 2;
const Int64 v5 = (Int64) 2;
const Int64 v6_0 = lv02;
const Int64 v6_1 = v6_0 / v4;
const Int64 v7 = v6_1 % v4;
const Int64 v8 = v6_0 % v4;
const float v9 = 0.5f * (float) ((Int64) -1 + v8);
const Word8 v10 = v9 > -0.5f && v9 <= 0.5f;
const float v11 = 0.5f * (float) ((Int64) -1 + v7);
const Word8 v12 = v11 > -0.5f && v11 <= 0.5f;
const Int64 v13 = (Int64) 2;
const Int64 v14 = (Int64) 2;
const float v15 = (v10 ? 1.0f : 0.0f) * (v12 ? 1.0f : 0.0f);
const float v16 = (float) (v8 - idiv(v13, (Int64) 2));
const float v17 = (float) (v7 - idiv(v14, (Int64) 2));
const Int64 v18 = (Int64) floorf((float) sh1 + v17);
const Int64 v19 = (Int64) floorf((float) sh0 + v16);
const Int64 v20 = max((Int64) 0, min(v18, (Int64) -1 + shIn0_1)) * shIn0_0 + max((Int64) 0, min(v19, (Int64) -1 + shIn0_0));

lv22 = (Int64) 1 + lv02;
lv21 = lv01 + arrIn0_0[v20] * v15;
lv20 = lv00 + v15;
lv02 = lv22;
lv01 = lv21;
lv00 = lv20;

const Int64 v3 = (Int64) 2;

lv10 = lv02 < v3 * v3;
}
arrOut_0[ix] = lv01 / lv00;
}
}


1.80:cc: (3.0,".\GS\169\220V \NUL\134I\169\"}\165\rTW")
#include <accelerate_cuda.h>
extern "C" __global__ void map(const Int64 shIn0_1, const Int64 shIn0_0, const float* __restrict__ arrIn0_0, const Int64 shOut_1, const Int64 shOut_0, Word8* __restrict__ arrOut_0)
{
const int shapeSize = shOut_1 * shOut_0;
const int gridSize = blockDim.x * gridDim.x;
int ix;

for (ix = blockDim.x * blockIdx.x + threadIdx.x; ix < shapeSize; ix += gridSize) {
const float x0 = arrIn0_0[ix];
const Word8 v0 = x0 > 1.0f;
const Word8 v1 = x0 < 0.0f;

arrOut_0[ix] = (Word8) truncf(255.0f * (v0 ? 1.0f : v1 ? 0.0f : x0));
}
}


1.80:cc: (3.0,"'\219\190.O\231F\SI/\203\213V\SOH\154\&7d")
#include <accelerate_cuda.h>
extern "C" __global__ void generate(const Int64 shIn0_1, const Int64 shIn0_0, const float* __restrict__ arrIn0_3, const float* __restrict__ arrIn0_2, const float* __restrict__ arrIn0_1, const float* __restrict__ arrIn0_0, const Int64 shOut_1, const Int64 shOut_0, float* __restrict__ arrOut_0)
{
const int shapeSize = shOut_1 * shOut_0;
const int gridSize = blockDim.x * gridDim.x;
int ix;

for (ix = blockDim.x * blockIdx.x + threadIdx.x; ix < shapeSize; ix += gridSize) {
const Int64 tmp_0 = ix;
const Int64 tmp_1 = tmp_0 / shOut_0;
const Int64 sh1 = tmp_1 % shOut_1;
const Int64 sh0 = tmp_0 % shOut_0;
float lv00 = 0.0f;
float lv01 = 0.0f;
Int64 lv02 = (Int64) 0;
Word8 lv10;

lv10 = lv02 < v3 * v3;

const Int64 v3 = (Int64) 2;

while (lv10) {
Int64 lv22;
float lv21;
float lv20;
const Int64 v4 = (Int64) 2;
const Int64 v5 = (Int64) 2;
const Int64 v6_0 = lv02;
const Int64 v6_1 = v6_0 / v4;
const Int64 v7 = v6_1 % v4;
const Int64 v8 = v6_0 % v4;
const float v9 = 0.5f * (float) ((Int64) -1 + v8);
const Word8 v10 = v9 > -0.5f && v9 <= 0.5f;
const float v11 = 0.5f * (float) ((Int64) -1 + v7);
const Word8 v12 = v11 > -0.5f && v11 <= 0.5f;
const Int64 v13 = (Int64) 2;
const Int64 v14 = (Int64) 2;
const float v15 = (v10 ? 1.0f : 0.0f) * (v12 ? 1.0f : 0.0f);
const float v16 = (float) (v8 - idiv(v13, (Int64) 2));
const float v17 = (float) (v7 - idiv(v14, (Int64) 2));
const Int64 v18 = (Int64) floorf((float) sh1 + v17);
const Int64 v19 = (Int64) floorf((float) sh0 + v16);
const Int64 v20 = max((Int64) 0, min(v18, (Int64) -1 + shIn0_1)) * shIn0_0 + max((Int64) 0, min(v19, (Int64) -1 + shIn0_0));

lv22 = (Int64) 1 + lv02;
lv21 = lv01 + arrIn0_1[v20] * v15;
lv20 = lv00 + v15;
lv02 = lv22;
lv01 = lv21;
lv00 = lv20;

const Int64 v3 = (Int64) 2;

lv10 = lv02 < v3 * v3;
}
arrOut_0[ix] = lv01 / lv00;
}
}


1.80:cc: (3.0,"n)\242h!\238/K\147\bz\229\229\176\173h")
#include <accelerate_cuda.h>
extern "C" __global__ void generate(const Int64 shIn0_1, const Int64 shIn0_0, const float* __restrict__ arrIn0_3, const float* __restrict__ arrIn0_2, const float* __restrict__ arrIn0_1, const float* __restrict__ arrIn0_0, const Int64 shOut_1, const Int64 shOut_0, float* __restrict__ arrOut_0)
{
const int shapeSize = shOut_1 * shOut_0;
const int gridSize = blockDim.x * gridDim.x;
int ix;

for (ix = blockDim.x * blockIdx.x + threadIdx.x; ix < shapeSize; ix += gridSize) {
const Int64 tmp_0 = ix;
const Int64 tmp_1 = tmp_0 / shOut_0;
const Int64 sh1 = tmp_1 % shOut_1;
const Int64 sh0 = tmp_0 % shOut_0;
float lv00 = 0.0f;
float lv01 = 0.0f;
Int64 lv02 = (Int64) 0;
Word8 lv10;

lv10 = lv02 < v3 * v3;

const Int64 v3 = (Int64) 2;

while (lv10) {
Int64 lv22;
float lv21;
float lv20;
const Int64 v4 = (Int64) 2;
const Int64 v5 = (Int64) 2;
const Int64 v6_0 = lv02;
const Int64 v6_1 = v6_0 / v4;
const Int64 v7 = v6_1 % v4;
const Int64 v8 = v6_0 % v4;
const float v9 = 0.5f * (float) ((Int64) -1 + v8);
const Word8 v10 = v9 > -0.5f && v9 <= 0.5f;
const float v11 = 0.5f * (float) ((Int64) -1 + v7);
const Word8 v12 = v11 > -0.5f && v11 <= 0.5f;
const Int64 v13 = (Int64) 2;
const Int64 v14 = (Int64) 2;
const float v15 = (v10 ? 1.0f : 0.0f) * (v12 ? 1.0f : 0.0f);
const float v16 = (float) (v8 - idiv(v13, (Int64) 2));
const float v17 = (float) (v7 - idiv(v14, (Int64) 2));
const Int64 v18 = (Int64) floorf((float) sh1 + v17);
const Int64 v19 = (Int64) floorf((float) sh0 + v16);
const Int64 v20 = max((Int64) 0, min(v18, (Int64) -1 + shIn0_1)) * shIn0_0 + max((Int64) 0, min(v19, (Int64) -1 + shIn0_0));

lv22 = (Int64) 1 + lv02;
lv21 = lv01 + arrIn0_2[v20] * v15;
lv20 = lv00 + v15;
lv02 = lv22;
lv01 = lv21;
lv00 = lv20;

const Int64 v3 = (Int64) 2;

lv10 = lv02 < v3 * v3;
}
arrOut_0[ix] = lv01 / lv00;
}
}


1.80:cc: (3.0,"\231\&9\DC2\189}\DC47\163\185\133_\231Ioq\196")
#include <accelerate_cuda.h>
extern "C" __global__ void generate(const Int64 shIn0_1, const Int64 shIn0_0, const float* __restrict__ arrIn0_3, const float* __restrict__ arrIn0_2, const float* __restrict__ arrIn0_1, const float* __restrict__ arrIn0_0, const Int64 shOut_1, const Int64 shOut_0, float* __restrict__ arrOut_0)
{
const int shapeSize = shOut_1 * shOut_0;
const int gridSize = blockDim.x * gridDim.x;
int ix;

for (ix = blockDim.x * blockIdx.x + threadIdx.x; ix < shapeSize; ix += gridSize) {
const Int64 tmp_0 = ix;
const Int64 tmp_1 = tmp_0 / shOut_0;
const Int64 sh1 = tmp_1 % shOut_1;
const Int64 sh0 = tmp_0 % shOut_0;
float lv00 = 0.0f;
float lv01 = 0.0f;
Int64 lv02 = (Int64) 0;
Word8 lv10;

lv10 = lv02 < v3 * v3;

const Int64 v3 = (Int64) 2;

while (lv10) {
Int64 lv22;
float lv21;
float lv20;
const Int64 v4 = (Int64) 2;
const Int64 v5 = (Int64) 2;
const Int64 v6_0 = lv02;
const Int64 v6_1 = v6_0 / v4;
const Int64 v7 = v6_1 % v4;
const Int64 v8 = v6_0 % v4;
const float v9 = 0.5f * (float) ((Int64) -1 + v8);
const Word8 v10 = v9 > -0.5f && v9 <= 0.5f;
const float v11 = 0.5f * (float) ((Int64) -1 + v7);
const Word8 v12 = v11 > -0.5f && v11 <= 0.5f;
const Int64 v13 = (Int64) 2;
const Int64 v14 = (Int64) 2;
const float v15 = (v10 ? 1.0f : 0.0f) * (v12 ? 1.0f : 0.0f);
const float v16 = (float) (v8 - idiv(v13, (Int64) 2));
const float v17 = (float) (v7 - idiv(v14, (Int64) 2));
const Int64 v18 = (Int64) floorf((float) sh1 + v17);
const Int64 v19 = (Int64) floorf((float) sh0 + v16);
const Int64 v20 = max((Int64) 0, min(v18, (Int64) -1 + shIn0_1)) * shIn0_0 + max((Int64) 0, min(v19, (Int64) -1 + shIn0_0));

lv22 = (Int64) 1 + lv02;
lv21 = lv01 + arrIn0_3[v20] * v15;
lv20 = lv00 + v15;
lv02 = lv22;
lv01 = lv21;
lv00 = lv20;

const Int64 v3 = (Int64) 2;

lv10 = lv02 < v3 * v3;
}
arrOut_0[ix] = lv01 / lv00;
}
}


1.81:cc: (3.0,">\230\230}f&a^\140\213\158BkXG(")
#include <accelerate_cuda.h>
extern "C" __global__ void generate(const Int64 shIn0_1, const Int64 shIn0_0, const Word8* __restrict__ arrIn0_0, const Int64 shIn1_1, const Int64 shIn1_0, const Word8* __restrict__ arrIn1_0, const Int64 shIn2_1, const Int64 shIn2_0, const Word8* __restrict__ arrIn2_0, const Int64 shIn3_1, const Int64 shIn3_0, const Word8* __restrict__ arrIn3_0, const Int64 shOut_1, const Int64 shOut_0, Word32* __restrict__ arrOut_0)
{
const int shapeSize = shOut_1 * shOut_0;
const int gridSize = blockDim.x * gridDim.x;
int ix;

for (ix = blockDim.x * blockIdx.x + threadIdx.x; ix < shapeSize; ix += gridSize) {
const Int64 tmp_0 = ix;
const Int64 tmp_1 = tmp_0 / shOut_0;
const Int64 sh1 = tmp_1 % shOut_1;
const Int64 sh0 = tmp_0 % shOut_0;
const Int64 v0 = sh1 * shIn2_0 + sh0;
const Int64 v1 = sh1 * shIn3_0 + sh0;
const Int64 v2 = sh1 * shIn0_0 + sh0;
const Int64 v3 = sh1 * shIn1_0 + sh0;
const Word8 v4 = arrIn2_0[v0];
const Word8 v5 = arrIn3_0[v1];
const Word8 v6 = arrIn0_0[v2];
const Word8 v7 = arrIn1_0[v3];

arrOut_0[ix] = (Word32) v4 + (Word32) 256 * (Word32) v5 + (Word32) 65536 * (Word32) v6 + (Word32) 16777216 * (Word32) v7;
}
}


1.81:gc: lookup/not found: Array #854
1.81:gc: mallocArray: 1 MB
1.81:gc: malloc/new
1.81:gc: insert: Array #854
1.81:gc: lookup/not found: Array #851
1.81:gc: mallocArray: 1 MB
1.81:gc: malloc/new
1.81:gc: insert: Array #851
1.81:gc: lookup/not found: Array #850
1.81:gc: mallocArray: 1 MB
1.81:gc: malloc/new
1.81:gc: insert: Array #850
1.81:gc: lookup/not found: Array #849
1.81:gc: mallocArray: 1 MB
1.81:gc: malloc/new
1.81:gc: insert: Array #849
1.81:cc: waiting for nvcc...
/tmp/accelerate-cuda-26080/dragon26081.cu(18): error: identifier "v3" is undefined

/tmp/accelerate-cuda-26080/dragon26084.cu(18): error: identifier "v3" is undefined

/tmp/accelerate-cuda-26080/dragon26083.cu(18): error: identifier "v3" is undefined

1 error detected in the compilation of "/tmp/tmpxft_000065ef_00000000-6_dragon26081.cpp1.ii".
1 error detected in the compilation of "/tmp/tmpxft_000065ff_00000000-6_dragon26084.cpp1.ii".
/tmp/accelerate-cuda-26080/dragon26085.cu(18): error: identifier "v3" is undefined

1 error detected in the compilation of "/tmp/tmpxft_000065f7_00000000-6_dragon26083.cpp1.ii".
<interactive>: nvcc terminated abnormally (2)
<interactive>: nvcc terminated abnormally (2)
1 error detected in the compilation of "/tmp/tmpxft_00006603_00000000-6_dragon26085.cpp1.ii".
<interactive>: nvcc terminated abnormally (2)
<interactive>: nvcc terminated abnormally (2)
1.81:cc: queue: 1.708 s, execute: 1.708 s
     ... /opt/nvidia/cuda/bin/nvcc -I /home/ec2-user/Flowbox/dist/libs/graphics/graphics/share/x86_64-linux-ghc-7.8.3/accelerate-cuda-0.15.0.0/cubits -arch=sm_30 -cubin -o /tmp/accelerate-cuda-26080/dragon26086.cubin --disable-warnings -DNDEBUG -O3 -m64 /tmp/accelerate-cuda-26080/dragon26086.cu
1.82:cc: queue: 2.006 s, execute: 2.006 s
     ... /opt/nvidia/cuda/bin/nvcc -I /home/ec2-user/Flowbox/dist/libs/graphics/graphics/share/x86_64-linux-ghc-7.8.3/accelerate-cuda-0.15.0.0/cubits -arch=sm_30 -cubin -o /tmp/accelerate-cuda-26080/dragon26082.cubin --disable-warnings -DNDEBUG -O3 -m64 /tmp/accelerate-cuda-26080/dragon26082.cu
1.82:cc: queue: 2.089 s, execute: 2.089 s
     ... /opt/nvidia/cuda/bin/nvcc -I /home/ec2-user/Flowbox/dist/libs/graphics/graphics/share/x86_64-linux-ghc-7.8.3/accelerate-cuda-0.15.0.0/cubits -arch=sm_30 -cubin -o /tmp/accelerate-cuda-26080/dragon26080.cubin --disable-warnings -DNDEBUG -O3 -m64 /tmp/accelerate-cuda-26080/dragon26080.cu
1.82:cc: persist/save: /home/ec2-user/.accelerate/accelerate-cuda-0.15.0.0/cache/3.0/zr139zrza5zrb2Pzr211Fzr167zr197zr207ziszr208zr129tzrzr
1.82:cc: entry function 'map' used 17 registers, 0 bytes smem, 0 bytes lmem, 0 bytes cmem
     ... multiprocessor occupancy 100.0% : 2048 threads over 64 warps in 16 blocks
1.82:gc: lookup/found: Array #858
1.82:gc: lookup/found: Array #854
1.82:gc: lookup/found: Array #851
1.82:gc: lookup/found: Array #850
1.82:gc: lookup/found: Array #849
1.82:exec: map<<< 128, 128, 0 >>> gpu: 100.352 µs, cpu: 0.000 Ts
1.82:gc: lookup/not found: Array #848
1.82:gc: mallocArray: 4 MB
1.82:gc: malloc/new
1.82:gc: insert: Array #848
1.82:cc: waiting for nvcc...

HANGED HERE
-}
