---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import Flowbox.Graphics.Image.Color
import Flowbox.Graphics.Utils
import Flowbox.Prelude hiding (transform)

import Data.Array.Accelerate (Exp)

import Utils



input, output :: String
input = "lena.png"
output = "out.png"

ex :: Exp Float -> Exp Float -> IO ()
ex black ev = testFunction (exposure black ev) input output

--bp black = testFunction (blackpointConvert black) input output

--invertible black lift = testFunction (inverseBlackpointConvert lift . blackpointConvert black) input output
--invertibleWhite white gain = testFunction (inverseWhitepointConvert gain . whitepointConvert white) input output

--saturation v = testColor (saturate v) input output

--correct s c g g' o = testColor (colorCorrect s c g g' o) input output

--idHSL = testColor (toRGB . toHSL)

posterization :: Exp Float -> IO ()
posterization v = testFunction (posterize v) input output

hsvT :: IO ()
hsvT = testColor (hsvTool (range (358/360) (359/360)) (180 / 360) (90/360) (range 0 1) 0 0 (range 0 1) (0) 0) input output

--colorMat = testColor (colorMatrix ((1, 0, 0), (0, 0, 1), (0.5, 0, 1))) input output

-- transfer :: IO ()
-- transfer = do
--     (plainR :: Matrix2 Float, plainG, plainB, plainA) <- testLoadRGBA' "samples/transfer/scotland_plain.bmp"
--     (houseR :: Matrix2 Float, houseG, houseB, _) <- testLoadRGBA' "samples/transfer/scotland_house.bmp"
--     putStrLn "loaded"

--     let vectoredPlain = customReshape $ A.lift (accMatrix plainR, accMatrix plainG, accMatrix plainB)
--         vectoredHouse = customReshape $ A.lift (accMatrix houseR, accMatrix houseG, accMatrix houseB)

--     let (r, g, b) = A.unlift $ reshapeBack (A.shape $ accMatrix plainR) $ rhoGi vectoredHouse vectoredPlain

    -- testSaveRGBA' "out.png" (Delayed r) (Delayed g) (Delayed b) (plainA)

main :: IO ()
main = do
    ex (-0.32) 0.3