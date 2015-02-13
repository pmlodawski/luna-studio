module Main where

import Flowbox.Graphics.Mockup.Basic
import Flowbox.Graphics.Mockup.ColorCorrect
import Flowbox.Prelude as P
--import Flowbox.Graphics.Utils
import Flowbox.Graphics.Color.RGBA
import Flowbox.Prelude hiding (transform)
--import Data.Array.Accelerate (Exp)
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
    let neutralOff  = VPS $ RGBA 0 0 0 0 :: VPS (RGBA Float)
    --let gamma1      = RGBA 2 2 2 2 :: RGBA Float
    --let gain1       = RGBA 2 2 2 2 :: RGBA Float
    --let sat1        = RGBA 2 2 2 2 :: RGBA Float
    --let contrast1   = RGBA 1.2 1.2 1.2 1.2 :: RGBA Float
    let offset1     = VPS $ RGBA 0.5 0.5 0.5 0.5 :: VPS (RGBA Float)
    let out = colorCorrectLuna

                {- master -}     (neutralSCGG, neutralSCGG, neutralSCGG, neutralSCGG, neutralOff)
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
