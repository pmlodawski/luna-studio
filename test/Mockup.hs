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
input = "RGB.png"
output = "out.png"


main :: IO ()
main = do
    putStrLn "Mockup test"
    image <- loadImageLuna $ "samples/" P.++ input
    let neutralSCGG = RGBA 1 1 1 0 :: RGBA Double
    let neutralOff  = RGBA 0 0 0 0 :: RGBA Double
    let gamma1		= RGBA 2 2 2 2 :: RGBA Double
    let gain1		= RGBA 2 2 2 2 :: RGBA Double
    let sat1 		= RGBA 2 2 2 2 :: RGBA Double
    let contrast1   = RGBA 1.2 1.2 1.2 1.2 :: RGBA Double
    let offset1		= RGBA 0.1 0.1 0.1 0.1 :: RGBA Double
    let out = colorCorrectLuna' 
		    	neutralSCGG
		    	neutralSCGG
		    	neutralSCGG
		    	neutralSCGG
		    	offset1

		    	neutralSCGG 
		    	neutralSCGG 
		    	neutralSCGG 
		    	neutralSCGG 
		    	neutralOff 

		    	neutralSCGG 
		    	neutralSCGG 
		    	neutralSCGG 
		    	neutralSCGG 
		    	neutralOff 

		    	neutralSCGG 
		    	neutralSCGG 
		    	neutralSCGG 
		    	neutralSCGG 
		    	neutralOff 

		    	image
    saveImageLuna output out
    putStrLn "Done"