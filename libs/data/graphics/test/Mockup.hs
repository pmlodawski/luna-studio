module Main where

import           Data.Array.Accelerate ((:.)(..), Z(..))
import qualified Data.Array.Accelerate as A

import           Flowbox.Graphics.Color.RGBA
import           Flowbox.Math.Function.Accelerate.BSpline as BSpline
import qualified Flowbox.Math.Function.CurveGUI           as CurveGUI
import           Flowbox.Prelude as P hiding (transform)

import Flowbox.Graphics.Mockup
import Flowbox.Graphics.Mockup.Basic
import Flowbox.Graphics.Mockup.ColorCorrect
import Utils



input, output :: String
input = "lena.png"
--input = "test_grad.png"
output = "out.png"
output1 = "out1.png"


main :: IO ()
main = do
    putStrLn "Mockup test"
    --image <- loadImageLuna $ "samples/" P.++ input
    --let out1 = onEach (+0.1) (+0.1) (+0.1) id image

    --saveImageLuna output1 out1

    image <- loadImageLuna $ "samples/" P.++ input
    let neutralSCGG = VPS $ RGBA 1 1 1 1 :: VPS (RGBA Float)
        neutralOff  = VPS $ RGBA 0 0 0 0 :: VPS (RGBA Float)

    let curveShadows    = makeSpline [BSplineNode (Point2 0 1) (Point2 (-1) 1) (Point2 0.03 1), BSplineNode (Point2 0.09 0) (Point2 0.06 0) (Point2 1.09 0)] :: BSpline Float
        --curveHighlights = makeSpline [BSplineNode (Point2 0.5 0) (Point2 (-0.5) 0) (Point2 (2/3) 0), BSplineNode (Point2 1 1) (Point2 (5/6) 1) (Point2 2 1)]
        curveHighlights = makeSpline [BSplineNode (Point2 0 0) (Point2 (-1) 0) (Point2 (1/3) 0), BSplineNode (Point2 1 1) (Point2 (2/3) 1) (Point2 2 1)] :: BSpline Float
        --curveHighlights = makeSpline [BSplineNode (Point2 0 1) (Point2 (-1) 1) (Point2 (1/3) 1), BSplineNode (Point2 1 1) (Point2 (2/3) 1) (Point2 2 1)]
        makeSpline      = A.fromList (Z :. 2)

    let out = colorCorrectLuna --Base (curveShadows, curveHighlights) -- TODO[KM]: remake this to use the same type of CurveGUI as Luna does (and the function colorCorrectLunaCurves)

                {- master -}     (neutralSCGG, neutralSCGG, neutralSCGG, neutralOff, neutralOff)
                {- shadows -}    (neutralSCGG, neutralSCGG, neutralSCGG, neutralSCGG, neutralOff)
                {- midtones -}   (neutralSCGG, neutralSCGG, neutralSCGG, neutralSCGG, neutralOff)
                {- highlights -} (neutralSCGG, neutralSCGG, neutralSCGG, neutralSCGG, neutralSCGG)

                image

    --let out2 = colorCorrectLuna
    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Float)
    --            (RGBA 0.8 0.8 0.8 0.8 :: RGBA Float)
    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Float)
    --            (RGBA 1 1 1 1 :: RGBA Float)
    --            (RGBA 0.07 0.07 0.07 0.07  :: RGBA Float)

    --            (RGBA 1.7 1.7 1.7 1.7 :: RGBA Float)
    --            (RGBA 0.56 0.56 0.56 0.56 :: RGBA Float)
    --            (RGBA 1.25 1.25 1.25 1.25 :: RGBA Float)
    --            (RGBA 0.62 0.62 0.62 0.62 :: RGBA Float)
    --            (RGBA 0.02 0.02 0.02 0.02 :: RGBA Float)

    --            (RGBA 0.56 0.56 0.56 0.56 :: RGBA Float)
    --            (RGBA 1.1 1.1 1.1 1.1 :: RGBA Float)
    --            (RGBA 1.35 1.35 1.35 1.35 :: RGBA Float)
    --            (RGBA 0.5 0.5 0.5 0.5 :: RGBA Float)
    --            (RGBA 0.015 0.015 0.015 0.015 :: RGBA Float)

    --            (RGBA 0.58 0.58 0.58 0.58 :: RGBA Float)
    --            (RGBA 1.45 1.45 1.45 1.45 :: RGBA Float)
    --            (RGBA 0.76 0.76 0.76 0.76 :: RGBA Float)
    --            (RGBA 0.76 0.76 0.76 0.76 :: RGBA Float)
    --            (RGBA 1.45 1.45 1.45 1.45 :: RGBA Float)
    --            (RGBA 0.07 0.07 0.07 0.07  :: RGBA Float)
    --            image

    --let out3 = colorCorrectLuna
    --            (RGBA 2 2 2 2 :: RGBA Float)
    --            (RGBA 0.5 0.5 0.5 0.5 :: RGBA Float)
    --            (RGBA 2 2 2 2 :: RGBA Float)
    --            (RGBA 0.2 0.2 0.2 0.2 :: RGBA Float)
    --            (RGBA 0 0 0 0 :: RGBA Float)

    --            neutralSCGG
    --            neutralSCGG
    --            neutralSCGG
    --            neutralSCGG
    --            neutralOff

    --            neutralSCGG
    --            neutralSCGG
    --            neutralSCGG
    --            neutralSCGG
    --            neutralOff

    --            neutralSCGG
    --            neutralSCGG
    --            neutralSCGG
    --            neutralSCGG
    --            neutralOff
    --            image

    --let out4 = colorCorrectLuna
    --            neutralSCGG
    --            neutralSCGG
    --            neutralSCGG
    --            neutralSCGG
    --            neutralOff

    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Float)
    --            (RGBA 0.8 0.8 0.8 0.8 :: RGBA Float)
    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Float)
    --            (RGBA 1 1 1 1 :: RGBA Float)
    --            (RGBA 0.07 0.07 0.07 0.07  :: RGBA Float)

    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Float)
    --            (RGBA 0.8 0.8 0.8 0.8 :: RGBA Float)
    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Float)
    --            (RGBA 1 1 1 1 :: RGBA Float)
    --            (RGBA 0.07 0.07 0.07 0.07  :: RGBA Float)

    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Float)
    --            (RGBA 0.8 0.8 0.8 0.8 :: RGBA Float)
    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Float)
    --            (RGBA 1 1 1 1 :: RGBA Float)
    --            (RGBA 0.07 0.07 0.07 0.07  :: RGBA Float)
    --            image

    --saveImageLuna "master.png" out3
    --saveImageLuna "hms.png" out4

    saveImageLuna output out
    putStrLn "Done"
