module Main where

import           Data.Array.Accelerate                    ((:.) (..), Z (..))
import qualified Data.Array.Accelerate                    as A

import           Flowbox.Geom2D.Rectangle                 (Rectangle (..))
import           Flowbox.Graphics.Color.RGBA
import           Flowbox.Math.Function.Accelerate.BSpline as BSpline
import qualified Flowbox.Math.Function.CurveGUI           as CurveGUI
import           Flowbox.Prelude                          as P hiding (transform)

import           Flowbox.Graphics.Mockup
import           Flowbox.Graphics.Mockup.Basic
import           Flowbox.Graphics.Mockup.ColorCorrect
import           Flowbox.Graphics.Mockup.Transform
import           Utils



input, output :: String
input = "lena.png"
--input = "test_grad.png"
output = "out.png"
output1 = "out1.png"


main :: IO ()
main = do
    putStrLn "Mockup test"

    image <- loadImageLuna $ "samples/" P.++ input
    let neutralSCGG = RGBA 1 1 1 1 :: RGBA Float
        neutralOff  = RGBA 0 0 0 0 :: RGBA Float

    let curveShadows    = makeSpline [BSplineNode (Point2 0 1) (Point2 (-1) 1) (Point2 0.03 1), BSplineNode (Point2 0.09 0) (Point2 0.06 0) (Point2 1.09 0)] :: BSpline Float
        --curveHighlights = makeSpline [BSplineNode (Point2 0.5 0) (Point2 (-0.5) 0) (Point2 (2/3) 0), BSplineNode (Point2 1 1) (Point2 (5/6) 1) (Point2 2 1)]
        curveHighlights = makeSpline [BSplineNode (Point2 0 0) (Point2 (-1) 0) (Point2 (1/3) 0), BSplineNode (Point2 1 1) (Point2 (2/3) 1) (Point2 2 1)] :: BSpline Float
        --curveHighlights = makeSpline [BSplineNode (Point2 0 1) (Point2 (-1) 1) (Point2 (1/3) 1), BSplineNode (Point2 1 1) (Point2 (2/3) 1) (Point2 2 1)]
        makeSpline      = A.fromList (Z :. 2)

    let outCC = colorCorrectLuna --Base (curveShadows, curveHighlights) -- TODO[KM]: remake this to use the same type of CurveGUI as Luna does (and the function colorCorrectLunaCurves)

                {- master -}     (ColorCC neutralSCGG neutralSCGG neutralSCGG neutralOff neutralOff)
                {- shadows -}    (ColorCC neutralSCGG neutralSCGG neutralSCGG neutralSCGG neutralOff)
                {- midtones -}   (ColorCC neutralSCGG neutralSCGG neutralSCGG neutralSCGG neutralOff)
                {- highlights -} (ColorCC neutralSCGG neutralSCGG neutralSCGG neutralSCGG neutralSCGG)

                image

    let outCrop = cropLuna (Rectangle (Point2 (-50) (-50)) (Point2 (250) (250))) True False image

    saveImageLuna output outCrop
    putStrLn "Done"
