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
    Image
    --erodeChannel,
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

--readImage :: String -> IO (Safe(Image Float))
--readImage fileIn = do
--    img <- either (\_ -> mempty) id `fmap` Image.readImageFromBMP fileIn
--    let Right img' = RGBA.decompose img
--        rgba = Image.reprFloat img'
--    return (Safe rgba)

--writeImage :: FilePath -> Image Float -> IO (Safe())
--writeImage file img = do
--    let Right img' = RGBA.compose $ Image.reprWord8 img
--    Image.writeImageToBMP (Interp.run) file img'
--    return $ Safe ()

----l_adjustCB :: A.Exp Float -> A.Exp Float -> Image Float -> IO (Safe(Image Float))
--adjustCB :: Double -> Double -> Image Float -> Image Float
----adjustCB :: A.Exp Float -> A.Exp Float -> Image Float -> IO (Image Float)
--adjustCB contrastValue brightnessValue img = img'
--    where Right img' = adjustCB_RGB (A.constant $ double2Float contrastValue) (A.constant $ double2Float brightnessValue) img

--convolve :: Double -> Image Float -> Image Float
--convolve kernel img = img'
--    where Right img' = convolveRGB convolve3x3 kernel' img
--          kernel' = map (A.constant . double2Float) $ replicate 9 kernel

--imgChannelGet :: String -> Image Float -> IO(Safe(Channel Float))
--imgChannelGet name img = do
--    let Right channel = Image.lookup name img
--    return (Safe channel)

--imgChannelInsert :: String -> Channel a -> Image a -> Image a
--imgChannelInsert = Image.insert

--channelMap :: (A.Elt a, A.Elt b) => (A.Exp a -> A.Exp b) -> Channel a -> Channel b
--channelMap = Channel.map

--type ImgF = Image Float

type ChannelF = Channel Float
