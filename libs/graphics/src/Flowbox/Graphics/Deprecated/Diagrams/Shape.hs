---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Deprecated.Diagrams.Shape where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Array.MArray      as MA
import           Data.Array.Accelerate  (Elt, IsFloating, Z(..))
import qualified Data.Array.Accelerate  as A
import           Data.Bits              ((.&.))
import           Data.Word              (Word32)
import qualified System.IO.Unsafe       as Unsafe

import           Diagrams.Prelude                (R2, SizeSpec2D, Diagram)
import qualified Diagrams.Prelude                as Diag
import           Diagrams.Backend.Cairo.Internal (Cairo, OutputType(..))
import qualified Diagrams.Backend.Cairo.Internal as Diag
import           Graphics.Rendering.Cairo        (Format(..), SurfaceData)
import qualified Graphics.Rendering.Cairo        as Cairo

--import           Flowbox.Graphics.Rendering.Cairo (Format(..), SurfaceData)
--import qualified Flowbox.Graphics.Rendering.Cairo as Cairo

import           Flowbox.Graphics.Image         (Image)
import qualified Flowbox.Graphics.Image         as Image
import           Flowbox.Graphics.Image.Channel (Channel2)
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Prelude                as P



rasterize :: (Elt a, IsFloating a, Eq a, Image img (Channel2 a))
    => Int -> Int -> Double -> Double -> SizeSpec2D -> Diagram Cairo R2 -> img (Channel2 a)
-- INFO: monadic version
rasterize w h x y size diagram = Unsafe.unsafePerformIO $ do
    pixels <- liftIO makeElements

    let converted = fmap convert pixels
        red       = channelAcc $ fmap (\(r, _, _, _) -> r) converted
        green     = channelAcc $ fmap (\(_, g, _, _) -> g) converted
        blue      = channelAcc $ fmap (\(_, _, b, _) -> b) converted
        alpha     = channelAcc $ fmap (\(_, _, _, a) -> a) converted

    return $ Image.insert "rgba.r" red
           $ Image.insert "rgba.g" green
           $ Image.insert "rgba.b" blue
           $ Image.insert "rgba.a" alpha
           mempty
    where makeElements = do
              let (_, render) = Diag.renderDia Diag.Cairo (Diag.CairoOptions "" size RenderOnly False) diagram
              surface <- Cairo.createImageSurface FormatARGB32 w h
              Cairo.surfaceSetDeviceOffset surface x y
              Cairo.renderWith surface render
              pixels <- Cairo.imageSurfaceGetPixels surface :: IO (SurfaceData Int Word32)
              MA.getElems pixels
          convert rgba = (r, g, b, a)
              where b = handleAlpha (fromIntegral (rgba .&. 0xFF) / 255)
                    g = handleAlpha (calculate 0x100)
                    r = handleAlpha (calculate 0x10000)
                    a = calculate 0x1000000
                    calculate val = fromIntegral ((rgba `div` val) .&. 0xFF) / 255
                    handleAlpha val = case a of
                        0  -> val
                        a' -> val / a'
          channelAcc chan = Channel.Acc $ A.use $ A.fromList (Z A.:. h A.:. w) chan

--rasterize w h x y size diagram = Image.insert "rgba.r" red $ Image.insert "rgba.g" green $ Image.insert "rgba.b" blue $ Image.insert "rgba.a" alpha $ mempty
--    where red       = channelAcc $ fmap (\(r, _, _, _) -> r) converted
--          green     = channelAcc $ fmap (\(_, g, _, _) -> g) converted
--          blue      = channelAcc $ fmap (\(_, _, b, _) -> b) converted
--          alpha     = channelAcc $ fmap (\(_, _, _, a) -> a) converted
--          channelAcc chan = Channel.Acc $ A.use $ A.fromList (Z A.:. h A.:. w) chan
--          converted = fmap convert pixels
--          convert rgba = (r, g, b, a)
--              where b = handleAlpha ((fromIntegral $ rgba .&. 0xFF) / 255)
--                    g = handleAlpha (calculate 0x100)
--                    r = handleAlpha (calculate 0x10000)
--                    a = calculate 0x1000000
--                    calculate val = (fromIntegral $ (rgba `div` val) .&. 0xFF) / 255
--                    handleAlpha val = case a of
--                        0  -> val
--                        a' -> val / a'
--          pixels = Cairo.getElems $ (Cairo.imageSurfaceGetPixels surface' :: SurfaceData Int Word32)
--          surface' = (renderWith (surfaceSetDeviceOffset surface))
--          renderWith surfaceIn = let tmp = Cairo.renderWith surfaceIn render in surfaceIn
--          surfaceSetDeviceOffset surfaceIn = let tmp = Cairo.surfaceSetDeviceOffset surfaceIn x y in surfaceIn
--          surface = Cairo.createImageSurface FormatARGB32 w h
--          (_, render) = Diag.renderDia Diag.Cairo (Diag.CairoOptions "" size RenderOnly False) diagram
