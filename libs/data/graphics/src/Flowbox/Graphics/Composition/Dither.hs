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

module Flowbox.Graphics.Composition.Dither where

import qualified Data.Array.Accelerate              as A
import qualified Data.Array.Accelerate.Array.Sugar  as Sugar

import Data.Vector.Unboxed as VU
import Foreign.Storable

import Flowbox.Prelude     as P
import Flowbox.Math.Matrix as M

import Math.Coordinate.Cartesian

-- == Error diffusion table ==

type DiffusionTable = MMatrix VU.Vector

instance (Storable a, Unbox a) => UnsafeIndexable VU.Vector a where
    data MatValue VU.Vector a = IMValue a
    unsafeShapeIndex MMatrix{..} sh = IMValue value
        where linearIndex = Sugar.toIndex canvas sh
              value       =  vector VU.! linearIndex

    unsafeConstant a = IMValue a

instance HasGetter (MatValue VU.Vector a) a where
    get (IMValue g) = g

dither :: (Storable a, RealFrac a, Unbox a) => DiffusionTable a -> Int -> MImage a -> IO ()
dither dTable bits' img = do
    let bits = P.fromIntegral bits'
    let Z :. height :. width = M.canvas img
    let array x y = M.index A.Mirror img $ Point2 x y

    let update x y f = do
            oldv <- get $ array x y
            array x y $= f oldv

    let Z :. diffH :. diffW = canvas dTable
    let diffW2 = diffW `div` 2
    forM_ [0..height-1] $ \y -> do
        forM_ [0..width-1] $ \x -> do
            oldpixel <- M.get $ array x y
            let newpixel = P.fromIntegral (floor $ oldpixel * bits) / bits
            array x y $= newpixel
            let quant_error = oldpixel - newpixel

            forM_ [0..diffH-1] $ \dy -> do
                forM_ [0..diffW-1] $ \dx -> do
                    let dither = get $ unsafeShapeIndex dTable (Z :. dy :. dx)
                    update (x + dx - diffW2) (y + dy) ( + (quant_error * dither))

floydSteinberg :: (Unbox a, Fractional a) => DiffusionTable a
floydSteinberg = MMatrix [ 0   , 0   , 7/16
                         , 3/16, 5/16, 1/16
                         ] (Z :. 2 :. 3)
