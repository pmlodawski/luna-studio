---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Flowbox.Graphics.Composition.Dither where

import qualified Data.Array.Accelerate              as A
import qualified Data.Array.Accelerate.Array.Sugar  as Sugar

import Data.Vector.Unboxed as VU
import Foreign.Storable

import Flowbox.Prelude     as P
import Flowbox.Math.Matrix as M
import Flowbox.Math.Index

import Math.Coordinate.Cartesian
import Math.Space.Space

import Data.List
import Data.Function

-- == Error diffusion table ==

type DiffusionTable = BMatrix VU.Vector

instance (Storable a, Unbox a) => Boundable (DiffusionTable a) Int a where
    unsafeIndex2D BMatrix{..} (Point2 x y) = container VU.! linearIndex
        where linearIndex = Sugar.toIndex canvas (A.Z A.:. y A.:. x)
    boundary BMatrix{..} = Grid width height
        where A.Z A.:. height A.:. width = canvas

dither :: (Storable a, RealFrac a, Unbox a) => M.Boundary (MValue a) -> DiffusionTable a -> Int -> MImage a -> IO ()
dither bnd dTable bits' img = do
    let bits = P.fromIntegral bits'
    let Z :. height :. width = canvas img
    let array x y = boundedIndex2D bnd img $ Point2 x y

    let update x y f = do
            oldv <- get $ array x y
            array x y $= f oldv

    let Z :. diffH :. diffW = canvas dTable
    let diffW2 = diffW `div` 2

    forM_ [0..height-1] $ \y ->
        forM_ [0..width-1] $ \x -> do
            oldpixel <- M.get $ array x y
            let newpixel = P.fromIntegral (floor $ oldpixel * bits :: Int) / bits
            array x y $= newpixel
            let quant_error = oldpixel - newpixel

            forM_ [0..diffH-1] $ \dy ->
                forM_ [0..diffW-1] $ \dx -> do
                    let dither = unsafeIndex2D dTable (Point2 dx dy)
                    update (x + dx - diffW2) (y + dy) ( + (quant_error * dither))

floydSteinberg :: (Unbox a, Fractional a) => DiffusionTable a
floydSteinberg = BMatrix [ 0   , 0   , 7/16
                         , 3/16, 5/16, 1/16
                         ] (Z :. 2 :. 3)

brukes :: (Unbox a, Fractional a) => DiffusionTable a
brukes = BMatrix [ 0   , 0   , 0   , 8/32, 4/32
                 , 2/32, 4/32, 8/32, 4/32, 2/32
                 ] (Z :. 2 :. 5)

fan :: (Unbox a, Fractional a) => DiffusionTable a
fan = BMatrix [ 0   , 0   , 0   , 7/16
              , 1/16, 3/16, 5/16, 0
              ] (Z :. 2 :. 5)

jarvisJudiceNinke :: (Unbox a, Fractional a) => DiffusionTable a
jarvisJudiceNinke = BMatrix [ 0   , 0   , 0   , 7/48, 5/48
                            , 3/48, 5/48, 7/48, 5/48, 3/48
                            , 1/48, 3/48, 5/48, 3/48, 1/48
                            ] (Z :. 3 :. 5)

stucki :: (Unbox a, Fractional a) => DiffusionTable a
stucki = BMatrix [ 0   , 0   , 0   , 8/42, 4/42
                 , 2/42, 4/42, 8/42, 4/42, 2/42
                 , 1/42, 2/42, 4/42, 2/42, 1/42
                 ] (Z :. 3 :. 5)

sierra2 :: (Unbox a, Fractional a) => DiffusionTable a
sierra2 = BMatrix [ 0   , 0   , 0   , 4/16, 3/16
                  , 1/16, 2/16, 3/16, 2/16, 1/16
                  ] (Z :. 2 :. 5)

sierra3 :: (Unbox a, Fractional a) => DiffusionTable a
sierra3 = BMatrix [ 0   , 0   , 0   , 5/32, 3/32
                  , 2/32, 4/32, 5/32, 4/32, 2/32
                  , 0   , 2/32, 3/32, 2/32, 0
                  ] (Z :. 3 :. 5)

sierraLite :: (Unbox a, Fractional a) => DiffusionTable a
sierraLite = BMatrix [ 0  , 0  , 2/4
                     , 1/4, 1/4, 0
                     ] (Z :. 2 :. 3)

atkinson :: (Unbox a, Fractional a) => DiffusionTable a
atkinson = BMatrix [ 0  , 0  , 1/8, 1/8
                   , 1/8, 1/8, 1/8, 0
                   , 0  , 1/8, 0  , 0
                   ] (Z :. 3 :. 4)

shiauFan4 :: (Unbox a, Fractional a) => DiffusionTable a
shiauFan4 = BMatrix [ 0  , 0  , 0  , 4/8, 0
                    , 1/8, 1/8, 2/8, 0  , 0
                    ] (Z :. 2 :. 5) 

shiauFan5 :: (Unbox a, Fractional a) => DiffusionTable a
shiauFan5 = BMatrix [ 0   , 0   , 0   , 0   , 8/16, 0   , 0
                    , 1/16, 1/16, 2/16, 4/16, 0   , 0   , 0
                    ] (Z :. 2 :. 7)

bayerTable :: (Elt a, IsFloating a) => Int -> Matrix2 a
bayerTable n = M.map (\x -> A.fromIntegral x / A.fromIntegral (A.lift maxVal + 1)) $ M.fromList arraySize array
    where array = snd <$> sortBy (compare `on` fst) table :: [Int]
          arraySize = Z :. tableSize :. tableSize :: DIM2
          table = [1 .. maxVal] <&> \i -> (Z :. yIndex i tableSize 0 :. xIndex i tableSize 0, i) 
          maxVal = tableSize ^ 2
          tableSize = 2 ^ n
          xIndex i size shift = if size == 2 then shift + i2 else xIndex newi s2 (shift + i2 * s2)
              where s2 = size `div` 2
                    i2 = i `mod` 2
                    newi = ((i - 1) `div` 4) + 1
          yIndex i size shift = if size == 2 then shift + i2 else yIndex newi s2 (shift + i2 * s2)
              where s2 = size `div` 2
                    i2 = ((i + 2) `mod`  4) `div` 2
                    newi = ((i - 1) `div` 4) + 1

bayer :: (Elt a, IsFloating a) => Int -> Matrix2 a -> Matrix2 a
bayer n mat = M.generate (shape mat) $ \elem@(A.unlift -> Z :. y :. x :: EDIM2) -> 
        let bayerVal = boundedIndex2D A.Wrap mosaic $ Point2 x y
            oldpixel = mat M.! elem
        in A.fromIntegral (A.floor (oldpixel * bits + bayerVal) :: Exp Int) / bits
    where bits   = A.fromIntegral $ A.lift (2 ^ (n - 1) :: Int)
          mosaic = bayerTable n
