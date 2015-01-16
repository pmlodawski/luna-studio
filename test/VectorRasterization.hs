---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Data.Array.Accelerate hiding (fromIntegral)
import           Data.Array.Accelerate as A
import           Data.Array.Accelerate.IO
import           Data.ByteString hiding (head)
import           Data.VectorSpace

import           Flowbox.Geom2D.ControlPoint
import           Flowbox.Geom2D.Path
import           Flowbox.Geom2D.Mask
import           Flowbox.Geom2D.Rasterizer hiding (makePoints, makeSegments)
import           Flowbox.Geom2D.QuadraticBezier
import           Flowbox.Geom2D.QuadraticBezier.Conversion
import           Flowbox.Geom2D.Accelerate.QuadraticBezier.Solve
import           Flowbox.Graphics.Image.IO.BMP
import           Flowbox.Graphics.Mockup (saveImageLuna)
import           Flowbox.Prelude as P hiding ((#))
import           Flowbox.Math.Matrix

import           Math.Coordinate.Cartesian (Point2 (..))

import           Data.Array.Accelerate.CUDA



main = do
    let closed = True
        (w,h)  = (640, 480) :: (Int, Int)
        points =   [ ControlPoint (Point2 212 209) Nothing                             (Just $ Point2 (211-212) (114-209))
                   , ControlPoint (Point2 338 210) (Just $ Point2 (329-338) (109-210)) (Just $ Point2 (450-338) (211-210))
                   , ControlPoint (Point2 343 330) (Just $ Point2 (456-343) (331-330)) Nothing
                   ]
        feather =  [ ControlPoint (Point2 212 (209-40)) Nothing                        (Just $ Point2 (211-212) (114-209))
                   , ControlPoint (Point2 338 (210-40)) (Just $ Point2 (329-338) (109-210)) (Just $ Point2 (450-338) (211-210))
                   , ControlPoint (Point2 343 (330-40)) (Just $ Point2 (456-343) (331-330)) Nothing
                   ]
        --points1  = [ ControlPoint (Point2 212 209) Nothing Nothing
        --           , ControlPoint (Point2 338 210) Nothing Nothing
        --           , ControlPoint (Point2 343 330) Nothing Nothing
        --           ]
        --feather1 = [ ControlPoint (Point2 212 (209-40)) Nothing Nothing
        --           , ControlPoint (Point2 338 (210-40)) Nothing Nothing
        --           , ControlPoint (Point2 343 (330-40)) Nothing Nothing
        --           ]

    let pat = Path True points
        fea = Path True feather
        --pat1 = Path True points1
        --fea1 = Path True feather1

    P.putStrLn "Test rasterizeMaskWithFeathers no fea"

    let arrD = rasterizeMask w h $ (Mask pat Nothing)
        imgD = matrixToImage arrD

    saveImageLuna "fooD2.png" imgD

    P.putStrLn "Test rasterizeMaskWithFeathers"
    
    let arrD = rasterizeMask w h $ (Mask pat (Just fea))
        imgD = matrixToImage arrD

    saveImageLuna "fooD.png" imgD


    return ()







