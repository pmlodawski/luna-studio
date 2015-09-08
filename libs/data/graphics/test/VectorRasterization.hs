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
import Data.Thyme.Clock
import Data.Thyme.Time
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



pointsStraightSmall   = [ ControlPoint (Point2 136.668065 58.260504)  Nothing Nothing
                        , ControlPoint (Point2 286.668065 206.260504) Nothing Nothing
                        , ControlPoint (Point2 374.668065 76.260504)  Nothing Nothing
                        ]
feathersStraightSmall = [ ControlPoint (Point2 75.995120 28.297279)   Nothing Nothing
                        , ControlPoint (Point2 293.366284 233.596489) Nothing Nothing
                        , ControlPoint (Point2 434.615106 44.870165)  Nothing Nothing
                        ]
pointsCurvedSmall   = [ ControlPoint (Point2 136.668065 58.260504)  (Just (Point2 293.366284 233.596489)) (Just (Point2 444.615106 24.870165))
                      , ControlPoint (Point2 286.668065 206.260504) (Just (Point2 193.366284 233.596489)) (Just (Point2 134.615106 14.870165))
                      , ControlPoint (Point2 374.668065 76.260504)  (Just (Point2 93.362284  233.596489)) (Just (Point2 134.615106 34.870165))
                      ]
feathersCurvedSmall = [ ControlPoint (Point2 75.995120 28.297279)   (Just (Point2 293.366284 233.596489)) (Just (Point2 434.615106 24.870165))
                      , ControlPoint (Point2 293.366284 233.596489) (Just (Point2 193.366384 233.596489)) (Just (Point2 334.615106 34.870165))
                      , ControlPoint (Point2 434.615106 44.870165)  (Just (Point2 293.366284 233.596489)) (Just (Point2 114.615106 44.870165))
                      ]

helper w h pointPath featherPath = do
    P.putStrLn "------------------  Path -> Quadratics: "
    t1 <- getCurrentTime
    let p = makeCubics pointPath
    let a = convertCubicsToQuadratics 5 0.0001 p
    print $ seq (a) ()
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1

    P.putStrLn "------------------  Path -> Matrix"
    t1 <- getCurrentTime
    let p = pathToMatrix w h pointPath
    print $ case p of
                Raw m -> seq m ()
                Delayed m -> let calcu = run m in seq calcu ()
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1

    P.putStrLn "------------------  Full Computation: "
    t1 <- getCurrentTime
    let arr = rasterizeMask w h (Mask pointPath (Just featherPath))
    print $ case arr of
                Raw m -> seq m ()
                Delayed m -> let calcu = run m in seq calcu ()
    t2 <- getCurrentTime
    print $ diffUTCTime t2 t1


    return ()

profile = do
    let closed = True
        (wSmall,  hSmall ) = (480,  240 ) :: (Int, Int)
        (wMedium, hMedium) = (1920, 1080) :: (Int, Int)
        (wBig,    hBig   ) = (4086, 2048) :: (Int, Int)
        pointPathStraightSmall   = Path closed pointsStraightSmall
        featherPathStraightSmall = Path closed feathersStraightSmall
        pointPathCurvedSmall     = Path closed pointsCurvedSmall
        featherPathCurvedSmall   = Path closed feathersCurvedSmall

    P.putStrLn "--------------------------  Profiling VectorRasterization !! "

    P.putStrLn "----------------------  Small Image, Small Straight Path: "
    helper wSmall hSmall pointPathStraightSmall featherPathStraightSmall

    P.putStrLn "----------------------  Medium Image, Small Straight Path: "
    helper wMedium hMedium pointPathStraightSmall featherPathStraightSmall

    P.putStrLn "----------------------  Big Image, Small Straight Path: "
    helper wBig hBig pointPathStraightSmall featherPathStraightSmall

    P.putStrLn "----------------------  Small Image, Small Curved Path: "
    helper wSmall hSmall pointPathCurvedSmall featherPathCurvedSmall

    P.putStrLn "----------------------  Medium Image, Small Curved Path: "
    helper wMedium hMedium pointPathCurvedSmall featherPathCurvedSmall

    P.putStrLn "----------------------  Big Image, Small Curved Path: "
    helper wBig hBig pointPathCurvedSmall featherPathCurvedSmall

    return ()

main = do
    let closed = True
        (w,h)  = (640, 480) :: (Int, Int)
        --------------------------------------------------------------------------------------------------------------------------
        points =   [ ControlPoint (Point2 212 209) Nothing                             (Just $ Point2 (211-212) (114-209))
                   , ControlPoint (Point2 338 210) (Just $ Point2 (329-338) (109-210)) (Just $ Point2 (450-338) (211-210))
                   , ControlPoint (Point2 343 330) (Just $ Point2 (456-343) (331-330)) Nothing
                   ]
        feather =  [ ControlPoint (Point2 (212-212) (209-40-209)) Nothing                        (Just $ Point2 (211-212) (114-209))
                   , ControlPoint (Point2 (338-338) (210-40-210)) (Just $ Point2 (329-338) (109-210)) (Just $ Point2 (450-338) (211-210))
                   , ControlPoint (Point2 (343-343) (330-40-330)) (Just $ Point2 (456-343) (331-330)) Nothing
                   ]
        --------------------------------------------------------------------------------------------------------------------------
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
        pointGUI2  = [ ControlPoint (Point2 949.997926 792.111355) (Just (Point2 4.550483 6.645528)) (Just (Point2 (-4.550483) (-6.645528)))
                     , ControlPoint (Point2 970.379410 868.066984) Nothing Nothing
                     , ControlPoint (Point2 981.605809 899.358123) Nothing Nothing
                     , ControlPoint (Point2 1005.376449 936.936465) Nothing Nothing
                     , ControlPoint (Point2 1046.300413 866.547248) Nothing Nothing
                     , ControlPoint (Point2 1048.755851 825.623284) (Just (Point2 (-1.636959) 2.455438)) (Just (Point2 1.636959 (-2.455438)))
                     ]
        featherGUI2 = [ ControlPoint (Point2 916.128669 837.914480) (Just (Point2 (-3.012514) (-12.051881))) (Just (Point2 3.012514 12.051881))
                      , ControlPoint (Point2 1010.111075 860.015149) Nothing Nothing
                      , ControlPoint (Point2 981.605809 899.358123) Nothing Nothing
                      , ControlPoint (Point2 1005.376449 936.936465) Nothing Nothing
                      , ControlPoint (Point2 1046.300413 866.547248) Nothing Nothing
                      , ControlPoint (Point2 1025.632820 793.611354) (Just (Point2 (-1.636959) 2.455438)) (Just (Point2 1.636959 (-2.455438)))
                      ]
    P.putStrLn "Test rasterizeMask empty list --> fooD.png "

    let patd = Path True []
        fead = Path True []

    let arrD = rasterizeMask w h $ (Mask patd (Just fead))
        imgD = matrixToImage arrD

    saveImageLuna "fooD.png" imgD

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

    P.putStrLn "Test rasterizeMask GUI2 example --> fooD4.png"

    let patGUI = Path True pointGUI2
        feaGUI = Path True featherGUI2
        arrGUI = rasterizeMask 1920 1080 (Mask patGUI (Just feaGUI))
        imgGUI = matrixToImage arrGUI

    saveImageLuna "fooD4.png" imgGUI

    profile


    return ()








