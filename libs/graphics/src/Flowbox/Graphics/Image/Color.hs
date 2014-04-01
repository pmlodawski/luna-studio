---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Image.Color where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A
import           Data.Map              (Map)
import qualified Data.Map              as Map

import qualified Flowbox.Graphics.Color         as Color
import           Flowbox.Graphics.Image         (ImageAcc)
import qualified Flowbox.Graphics.Image         as Image
import qualified Flowbox.Graphics.Image.Channel as Channel
import qualified Flowbox.Graphics.Utils         as U
import           Flowbox.Prelude                as P


-- TODO: discuss whether those functions should require rgb color space or should it look
-- for other color spaces too

-- TODO: discuss:
-- img = open "ala.png"
-- img.rgb.r
-- should those functions encapsulate the Image type in something holding the info about the color space, so u can use the line above?

hsv :: (A.Elt a, A.IsFloating a, A.Shape ix) => ImageAcc ix a -> Either Image.Error (ImageAcc ix a)
hsv img = do
    r <- Image.get "rgba.r" img
    g <- Image.get "rgba.g" img
    b <- Image.get "rgba.b" img
    let outimg = Image.insert "hsv.h" hue
               $ Image.insert "hsv.s" saturation
               $ Image.insert "hsv.v" value
               $ img
        hue        = Channel.Acc $ A.map U.fstTrio hsv'
        saturation = Channel.Acc $ A.map U.sndTrio hsv'
        value      = Channel.Acc $ A.map U.trdTrio hsv'
        rgb = A.zip3 (Channel.accMatrix r) (Channel.accMatrix g) (Channel.accMatrix b)
        hsv' = A.map convertToHSV rgb
        convertToHSV rgb' = A.lift (h, s, v)
            where
                (r',g',b') = A.unlift rgb'
                Color.HSV h s v = Color.toHSV $ Color.RGB r' g' b'
    return outimg

hsl :: (A.Elt a, A.IsFloating a, A.Shape ix) => ImageAcc ix a -> Either Image.Error (ImageAcc ix a)
hsl img = do
    r <- Image.get "rgba.r" img
    g <- Image.get "rgba.g" img
    b <- Image.get "rgba.b" img
    let outimg = Image.insert "hsl.h" hue
               $ Image.insert "hsl.s" saturation
               $ Image.insert "hsl.l" value
               $ img
        hue        = Channel.Acc $ A.map U.fstTrio hsl'
        saturation = Channel.Acc $ A.map U.sndTrio hsl'
        value      = Channel.Acc $ A.map U.trdTrio hsl'
        rgb = A.zip3 (Channel.accMatrix r) (Channel.accMatrix g) (Channel.accMatrix b)
        hsl' = A.map convertToHSL rgb
        convertToHSL rgb' = A.lift (h, s, l)
            where
                (r',g',b') = A.unlift rgb'
                Color.HSL h s l = Color.toHSL $ Color.RGB r' g' b'
    return outimg

-- TODO: handle mask input
-- TODO: handle premultiplication and mix

math :: (A.Elt a, A.IsNum a, A.Shape ix)
    => (Exp a -> Exp a -> Exp a) -> ImageAcc ix a -> Map Channel.Name (Exp a)
    -> Maybe Image.Mask -> Maybe Image.Premultiply -> Double
    -> ImageAcc ix a
math f img values mask premultiply mix =
    Image.mapWithKey handleChan img
    where
        _ = (mask, premultiply, mix) -- FIXME: make a use of those
        handleChan name chan = case Map.lookup name values of
            Nothing -> chan
            Just value -> Channel.map (f value) chan

offset :: (A.Elt a, A.IsNum a, A.Shape ix)
    => ImageAcc ix a -> Map Channel.Name (Exp a)
    -> Maybe Image.Mask -> Maybe Image.Premultiply -> Double
    -> ImageAcc ix a
offset = math (+)

multiply :: (A.Elt a, A.IsNum a, A.Shape ix)
    => ImageAcc ix a -> Map Channel.Name (Exp a)
    -> Maybe Image.Mask -> Maybe Image.Premultiply -> Double
    -> ImageAcc ix a
multiply = math (*)

gamma :: (A.Elt a, A.IsFloating a, A.Shape ix)
    => ImageAcc ix a -> Map Channel.Name (Exp a)
    -> Maybe Image.Mask -> Maybe Image.Premultiply -> Double
    -> ImageAcc ix a
gamma = math (\v -> (**(1/v)))

-- TODO: test this comparing to nuke, there are some differences atm
-- probably the result in nuke is being calculated based on an AND|OR relation between all input channels
clamp :: (A.Shape ix, A.Elt a, A.IsScalar a)
    => ImageAcc ix a -> Map Channel.Name (U.Range (Exp a), Maybe (U.Range (Exp a)))
    -> Maybe Image.Mask -> Maybe Image.Premultiply -> Double
    -> ImageAcc ix a
clamp img ranges mask premultiply mix =
    Image.mapWithKey handleChan img
    where
        _ = (mask, premultiply, mix) -- FIXME: make a use of those
        handleChan name chan = case Map.lookup name ranges of
            Nothing -> chan
            Just value -> Channel.map (clip value) chan
        clip (U.Range lo hi, clampTo) v = case clampTo of
            Nothing -> v A.<* lo A.? (lo, v A.>* hi A.? (hi, v))
            Just (U.Range lo' hi') -> v A.<* lo A.? (lo', v A.>* hi A.? (hi', v))

-- INFO: in Nuke this does not have the OR relation between channels, it has something pretty weird
-- soooooo....... either fuck it and do it our way or... focus on it later on
clipTest :: (A.Shape ix, A.Elt a, A.IsNum a)
    => ImageAcc ix a -> Map Channel.Name (U.Range (Exp a))
    -> Maybe Image.Mask -> Maybe Image.Premultiply -> Double
    -> ImageAcc ix a
clipTest img ranges mask premultiply mix =
    Image.mapWithKey handleChan img
    where
        _ = (mask, premultiply, mix) -- FIXME: make a use of those
        handleChan name chan = case Map.lookup name ranges of
            Nothing -> chan
            Just _ -> Channel.zipWith zebraStripes chan matched
        zebraStripes x b = b A.? (1, x) -- TODO: this should make stripes, not constant color ;]
        matched = Image.foldrWithKey fold initialAcc filteredChans
        fold name chan acc = let Just (U.Range lo hi) = Map.lookup name ranges in Channel.zipWith (A.||*) acc (Channel.map (withinRange lo hi) chan)
        withinRange lo hi x = x A.<* lo A.||* x A.>* hi
        initialAcc = Channel.fill shape $ A.constant False
        shape = let (_, chan) = Image.elementAt 0 filteredChans in Channel.shape $ chan
        filteredChans = Image.filterByName (Map.keys ranges) img

