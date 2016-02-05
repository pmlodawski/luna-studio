{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Geom2D.Accelerate.QuadraticBezier.SolveSpec where

import           Data.Array.Accelerate                           as A
import           Data.Array.Accelerate.CUDA
import           Test.Hspec
import           Test.HUnit
import           Test.QuickCheck
import           TestHelpers

import           Flowbox.Geom2D.Accelerate.QuadraticBezier.Solve
import           Flowbox.Geom2D.QuadraticBezier

import           Flowbox.Prelude                                 as P

import           Math.Coordinate.Cartesian                       (Point2 (..))

spec :: Spec
spec = do
    describe "eee func" $ do
        it "should something" $
            pending


main :: IO ()
main = do
    print "cardano tests"

    let (n :: Exp Int, res :: Exp (Float, Float, Float)) = unlift res'
        res' = cardano 1 (-2) (-7.89) 12.96
    --print n
    print $ run $ unit res
    let (n :: Exp Int, res :: Exp (Float, Float, Float)) = unlift res'
        res' = cardano 1 1.3 (-7.5383) 5.44645
    --print n
    print $ run $ unit res
    let (n :: Exp Int, res :: Exp (Float, Float, Float)) = unlift res'
        res' = cardano 1 (-9.336) 29.0536 (-30.1383)
    --print n
    print $ run $ unit res
    let (n :: Exp Int, res :: Exp (Float, Float, Float)) = unlift res'
        res' = cardano 1 0 (-3) 2
    --print n
    print $ run $ unit res
    let (n :: Exp Int, res :: Exp (Float, Float, Float)) = unlift res'
        res' = cardano 5.5 (-34) 144 (-66)
    --print n
    print $ run $ unit res

    print "distance from quadratic tests"

    let p0 = Point2 0 0 :: Point2 Float
        p1 = Point2 1 0 :: Point2 Float
        p2 = Point2 2 0 :: Point2 Float
        m = Point2 1 1 :: Point2 Float
        bez = QuadraticBezier p0 p1 p2
    print $ run $ unit $ distanceFromQuadratic' (A.lift m) (A.lift bez)

    let p0 = Point2 136.66806 58.260506 :: Point2 Float
        p1 = Point2 211.66803 142.26051 :: Point2 Float
        p2 = Point2 286.66806 226.2605 :: Point2 Float
        m = Point2 211 142 :: Point2 Float
        bez = QuadraticBezier p0 p1 p2
    print $ run $ unit $ distanceFromQuadratic' (A.lift m) (A.lift bez)


    let p0 = Point2 630.0 470.0 :: Point2 Float
        p1 = Point2 320.0 470.0 :: Point2 Float
        p2 = Point2 10.0 470.0 :: Point2 Float
        m = Point2 120 469.55 :: Point2 Float
        bez = QuadraticBezier p0 p1 p2
    print $ run $ unit $ distanceFromQuadratic' (A.lift m) (A.lift bez)

    -- Point2 213.15137 152.2461, quadraticC1 = Point2 214.09656 144.13135, quadraticC2 = Point2 216.75781 137.625

    let p0 = Point2 213.15137 152.2461 :: Point2 Float
        p1 = Point2 214.09656 144.13135 :: Point2 Float
        p2 = Point2 216.75781 137.625 :: Point2 Float
        m = Point2 214.09656 144.13135 :: Point2 Float
        bez = QuadraticBezier p0 p1 p2
    print $ run $ unit $ distanceFromQuadratic' (A.lift m) (A.lift bez)

    return ()


