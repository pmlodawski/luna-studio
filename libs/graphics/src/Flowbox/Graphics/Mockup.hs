---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeOperators             #-}

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Graphics.Mockup (
    module Flowbox.Graphics.Mockup,
    Image,
    Channel,
    erodeChannel,
) where

import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Interpreter as Interp
import           GHC.Float
import           Luna.Target.HS.Core               hiding (print, return)

import           Flowbox.Graphics.Algorithms
import           Flowbox.Graphics.Raster.Image     (Image)
import qualified Flowbox.Graphics.Raster.Image     as Image
import           Flowbox.Graphics.Raster.Channel   (Channel)
import qualified Flowbox.Graphics.Raster.Channel   as Channel
import qualified Flowbox.Graphics.Raster.IO        as Image
import qualified Flowbox.Graphics.Raster.Repr.RGBA as RGBA
import           Flowbox.Prelude                   hiding ((.))


testm :: Num a => a -> a
testm x = x*3

readImage :: String -> IO (Safe(Image Double))
readImage fileIn = do
    img <- either (\_ -> mempty) id `fmap` Image.readImageFromBMP fileIn
    let Right img' = RGBA.decompose img
        rgba = Image.reprDouble img'
    return (Safe rgba)

writeImage :: FilePath -> Image Double -> IO (Safe())
writeImage file img = do
    let Right img' = RGBA.compose $ Image.reprWord8 img
    Image.writeImageToBMP (Interp.run) file img'
    return $ Safe ()

adjustCB :: Double -> Double -> Image Double -> Image Double
adjustCB contrastValue brightnessValue img = img'
    where Right img' = adjustCB_RGB (A.constant contrastValue) (A.constant brightnessValue) img

convolve :: Double -> Image Double -> Image Double
convolve kernel img = img'
    where Right img' = convolveRGB convolve3x3 kernel' img
          kernel' = map A.constant $ replicate 9 kernel

imgChannelGet :: String -> Image Double -> Channel Double
imgChannelGet name img = channel
    where Right channel = Image.lookup name img

imgChannelInsert :: String -> Channel Double -> Image Double -> Image Double
imgChannelInsert = Image.insert

channelMap :: (A.Exp Double -> A.Exp Double) -> Channel Double -> Channel Double
channelMap = Channel.map

constant :: Double -> A.Exp Double
constant = A.constant
