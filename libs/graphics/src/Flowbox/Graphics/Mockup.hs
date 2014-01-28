{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeOperators             #-}

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Graphics.Mockup (
    module Flowbox.Graphics.Mockup,
    writeImageToBMP
) where

import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Interpreter as Interp
import           GHC.Float
import           Luna.Target.HS.Core               hiding (print, return)

import           Flowbox.Graphics.Algorithms
import           Flowbox.Graphics.Raster.Image     (Image)
import qualified Flowbox.Graphics.Raster.Image     as Image
import           Flowbox.Graphics.Raster.IO        (writeImageToBMP)
import qualified Flowbox.Graphics.Raster.IO        as Image
import qualified Flowbox.Graphics.Raster.Repr.RGBA as RGBA
import           Flowbox.Prelude                   hiding ((.))

testm :: Num a => a -> a
testm x = x*3

readImage :: String -> IO (Safe(Image Float))
readImage fileIn = do
    img <- either (\_ -> mempty) id `fmap` Image.readImageFromBMP fileIn
    let Right img' = RGBA.decompose img
        rgba = Image.reprFloat img'
    return (Safe rgba)

writeImage :: FilePath -> Image Float -> IO (Safe())
writeImage file img = do
    let Right img' = RGBA.compose $ Image.reprWord8 img
    writeImageToBMP (Interp.run) file img'
    return $ Safe ()

--l_adjustCB :: A.Exp Float -> A.Exp Float -> Image Float -> IO (Safe(Image Float))
adjustCB :: Double -> Double -> Image Float -> IO (Safe(Image Float))
--adjustCB :: A.Exp Float -> A.Exp Float -> Image Float -> IO (Image Float)
adjustCB contrastValue brightnessValue img = do
    let Right img' = adjustCB_RGB (A.constant $ double2Float contrastValue) (A.constant $ double2Float brightnessValue) img
    return (Safe img')

convolve :: Double -> Image Float -> IO (Safe(Image Float))
convolve kernel img = do
    let Right img' = convolveRGB convolve3x3 kernel' img
        --kernel' = map (A.constant . double2Float) kernel
        kernel' = map (A.constant . double2Float) $ replicate 9 kernel
    return (Safe img')
