---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import Flowbox.Graphics.Mockup
import Flowbox.Prelude as P
--import Flowbox.Graphics.Utils
import Flowbox.Graphics.Color.RGBA
import Flowbox.Prelude hiding (transform)
--import Data.Array.Accelerate (Exp)
import Utils



input, output :: String
input = "lena.png"
output = "out.png"
output1 = "out1.png"


main :: IO ()
main = do
    putStrLn "Mockup test"
    --image <- loadImageLuna $ "samples/" P.++ input
    --let out1 = onEach (+0.1) (+0.1) (+0.1) id image
    
    --saveImageLuna output1 out1

    --image <- loadImageLuna $ "samples/" P.++ input
    --let neutralSCGG = RGBA 1 1 1 0 :: RGBA Double
    --let neutralOff  = RGBA 0 0 0 0 :: RGBA Double
    --let gamma1      = RGBA 2 2 2 2 :: RGBA Double
    --let gain1       = RGBA 2 2 2 2 :: RGBA Double
    --let sat1        = RGBA 2 2 2 2 :: RGBA Double
    --let contrast1   = RGBA 1.2 1.2 1.2 1.2 :: RGBA Double
    --let offset1     = RGBA 0.1 0.1 0.1 0.1 :: RGBA Double
    --let out = colorCorrectLuna' 
    --            neutralSCGG
    --            neutralSCGG
    --            neutralSCGG
    --            neutralSCGG
    --            offset1

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

    --let out2 = colorCorrectLuna'
    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Double)
    --            (RGBA 0.8 0.8 0.8 0.8 :: RGBA Double)
    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Double)
    --            (RGBA 1 1 1 1 :: RGBA Double)
    --            (RGBA 0.07 0.07 0.07 0.07  :: RGBA Double)

    --            (RGBA 1.7 1.7 1.7 1.7 :: RGBA Double)
    --            (RGBA 0.56 0.56 0.56 0.56 :: RGBA Double)
    --            (RGBA 1.25 1.25 1.25 1.25 :: RGBA Double)
    --            (RGBA 0.62 0.62 0.62 0.62 :: RGBA Double)
    --            (RGBA 0.02 0.02 0.02 0.02 :: RGBA Double)

    --            (RGBA 0.56 0.56 0.56 0.56 :: RGBA Double)
    --            (RGBA 1.1 1.1 1.1 1.1 :: RGBA Double)
    --            (RGBA 1.35 1.35 1.35 1.35 :: RGBA Double)
    --            (RGBA 0.5 0.5 0.5 0.5 :: RGBA Double)
    --            (RGBA 0.015 0.015 0.015 0.015 :: RGBA Double)

    --            (RGBA 0.58 0.58 0.58 0.58 :: RGBA Double)
    --            (RGBA 1.45 1.45 1.45 1.45 :: RGBA Double)
    --            (RGBA 0.76 0.76 0.76 0.76 :: RGBA Double)
    --            (RGBA 1.45 1.45 1.45 1.45 :: RGBA Double)
    --            (RGBA 0.07 0.07 0.07 0.07  :: RGBA Double)
    --            image

    --let out3 = colorCorrectLuna'
    --            (RGBA 2 2 2 2 :: RGBA Double)
    --            (RGBA 0.5 0.5 0.5 0.5 :: RGBA Double)
    --            (RGBA 2 2 2 2 :: RGBA Double)
    --            (RGBA 0.2 0.2 0.2 0.2 :: RGBA Double)
    --            (RGBA 0 0 0 0 :: RGBA Double)

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

    --let out4 = colorCorrectLuna'
    --            neutralSCGG 
    --            neutralSCGG 
    --            neutralSCGG 
    --            neutralSCGG 
    --            neutralOff

    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Double)
    --            (RGBA 0.8 0.8 0.8 0.8 :: RGBA Double)
    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Double)
    --            (RGBA 1 1 1 1 :: RGBA Double)
    --            (RGBA 0.07 0.07 0.07 0.07  :: RGBA Double)

    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Double)
    --            (RGBA 0.8 0.8 0.8 0.8 :: RGBA Double)
    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Double)
    --            (RGBA 1 1 1 1 :: RGBA Double)
    --            (RGBA 0.07 0.07 0.07 0.07  :: RGBA Double)

    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Double)
    --            (RGBA 0.8 0.8 0.8 0.8 :: RGBA Double)
    --            (RGBA 1.2 1.2 1.2 1.2 :: RGBA Double)
    --            (RGBA 1 1 1 1 :: RGBA Double)
    --            (RGBA 0.07 0.07 0.07 0.07  :: RGBA Double)
    --            image

    --saveImageLuna "master.png" out3
    --saveImageLuna "hms.png" out4

    --saveImageLuna output out3
    --putStrLn "Done"