---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Data.Array.Accelerate    hiding (fromIntegral)
import Data.Array.Accelerate    as A
import Data.Array.Accelerate.IO
import Data.ByteString          hiding (head)
import Data.VectorSpace

import Flowbox.Geom2D.Accelerate.QuadraticBezier.Solve
import Flowbox.Geom2D.ControlPoint
import Flowbox.Geom2D.Mask
import Flowbox.Geom2D.Path
import Flowbox.Geom2D.QuadraticBezier
import Flowbox.Geom2D.QuadraticBezier.Conversion
import Flowbox.Geom2D.Rasterizer                       hiding (makePoints, makeSegments)
import Flowbox.Graphics.Image.IO.BMP
import Flowbox.Graphics.Mockup.Basic                   (saveImageLuna)
import Flowbox.Math.Matrix
import Flowbox.Prelude                                 as P hiding (( # ))

import Math.Coordinate.Cartesian (Point2 (..))

import Data.Array.Accelerate.CUDA



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
        points1  = [ ControlPoint (Point2 212 209) Nothing Nothing
                   , ControlPoint (Point2 338 210) Nothing Nothing
                   , ControlPoint (Point2 343 330) Nothing Nothing
                   ]
        feather1 = [ ControlPoint (Point2 212 (209-40)) Nothing Nothing
                   , ControlPoint (Point2 338 (210-40)) Nothing Nothing
                   , ControlPoint (Point2 343 (330-40)) Nothing Nothing
                   ]
        pointsd  = [ ControlPoint (Point2 10 10) Nothing Nothing
                   , ControlPoint (Point2 630 10)  Nothing Nothing
                   , ControlPoint (Point2 630 470) Nothing Nothing
                   , ControlPoint (Point2 10  470) Nothing Nothing
                   ]
        featherd = [ ControlPoint (Point2 10 10) Nothing Nothing
                   , ControlPoint (Point2 630 10)  Nothing Nothing
                   , ControlPoint (Point2 630 470) Nothing Nothing
                   , ControlPoint (Point2 10  470) Nothing Nothing
                   ]
        pointsGUI  = [ ControlPoint (Point2 136.668065 58.260504)  Nothing Nothing
                     , ControlPoint (Point2 286.668065 226.260504) Nothing Nothing
                     , ControlPoint (Point2 374.668065 76.260504)  Nothing Nothing
                     ]
        featherGUI = [ ControlPoint (Point2 75.995120 28.297279)   Nothing Nothing
                     , ControlPoint (Point2 293.366284 293.596489) Nothing Nothing
                     , ControlPoint (Point2 434.615106 44.870165)  Nothing Nothing
                     ]

    P.putStrLn "Test rasterizeMask one line, no handles --> foo.png"

    let patd = Path True pointsd
        fead = Path True featherd

    let arrD = rasterizeMask w h $ (Mask patd (Just fead))
        imgD = matrixToImage arrD

    saveImageLuna "foo.png" imgD

    P.putStrLn "Test rasterizeMask no fea, no handles --> foo1.png"

    let pat1 = Path True points1
        fea1 = Path True feather1

    let arrD = rasterizeMask w h $ (Mask pat1 Nothing)
        imgD = matrixToImage arrD

    saveImageLuna "foo1.png" imgD

    P.putStrLn "Test rasterizeMask no handles --> foo2.png"

    let arrD = rasterizeMask w h $ (Mask pat1 (Just fea1))
        imgD = matrixToImage arrD

    saveImageLuna "foo2.png" imgD

    P.putStrLn "Test rasterizeMask no fea --> fooD1.png"

    let pat = Path True points
        fea = Path True feather

    let arrD = rasterizeMask w h $ (Mask pat Nothing)
        imgD = matrixToImage arrD

    saveImageLuna "fooD1.png" imgD

    P.putStrLn "Test rasterizeMask --> fooD2.png"

    let arrD = rasterizeMask w h $ (Mask pat (Just fea))
        imgD = matrixToImage arrD

    saveImageLuna "fooD2.png" imgD

    P.putStrLn "Test rasterizeMask GUI example --> fooD3.png"

    let patGUI = Path True pointsGUI
        feaGUI = Path True featherGUI
        arrGUI = rasterizeMask w h (Mask patGUI (Just feaGUI))
        imgGUI = matrixToImage arrGUI

    saveImageLuna "fooD3.png" imgGUI




    return ()







