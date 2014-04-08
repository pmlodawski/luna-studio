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

--import qualified Debug.Trace as Dbg

import qualified Flowbox.Graphics.Color             as Color
import           Flowbox.Graphics.Image             (ImageAcc)
import qualified Flowbox.Graphics.Image             as Image
import qualified Flowbox.Graphics.Image.Channel     as Channel
import           Flowbox.Graphics.Image.Composition (Premultiply(..), Mask(..), Clamp)
import qualified Flowbox.Graphics.Image.Composition as Comp
import qualified Flowbox.Graphics.Image.Merge       as Merge
import           Flowbox.Graphics.Utils             (Range(..))
import qualified Flowbox.Graphics.Utils             as U
import           Flowbox.Prelude                    as P


-- TODO: discuss whether those functions should require rgb color space or should it look
-- for other color spaces too

-- TODO: discuss:
-- img = open "ala.png"
-- img.rgb.r
-- should those functions encapsulate the Image type in something holding the info about the color space, so u can use the line above?

hsv :: (A.Elt a, A.IsFloating a, A.Shape ix) => ImageAcc ix a -> Image.Result (ImageAcc ix a)
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

hsl :: (A.Elt a, A.IsFloating a, A.Shape ix) => ImageAcc ix a -> Image.Result (ImageAcc ix a)
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
            where (r',g',b')      = A.unlift rgb'
                  Color.HSL h s l = Color.toHSL $ Color.RGB r' g' b'
    return outimg


math :: (A.Elt a, A.IsFloating a, A.Shape ix)
    => (Exp a -> Exp a -> Exp a) -> ImageAcc ix a -> Map Channel.Name (Exp a)
    -> Maybe (Mask ix a) -> Maybe Premultiply -> Exp a -- TODO: add to wiki why this line is separated from others
    -> Image.Result (ImageAcc ix a)
math f img values maskInfo premultiply mixValue = do
    unpremultiplied <- Comp.unpremultiply img premultiply
    result          <- Comp.premultiply (Image.mapWithKey handleChan unpremultiplied) premultiply
    resultMixed     <- Merge.mix' img result mixValue
    Merge.mask resultMixed img maskInfo
    where handleChan name chan = case Map.lookup name values of
              Nothing    -> chan
              Just value -> Channel.map (\x -> f value x) chan

offset :: (A.Elt a, A.IsFloating a, A.Shape ix)
    => ImageAcc ix a -> Map Channel.Name (Exp a)
    -> Maybe (Mask ix a) -> Maybe Premultiply -> Exp a
    -> Image.Result (ImageAcc ix a)
offset = math (+)

multiply :: (A.Elt a, A.IsFloating a, A.Shape ix)
    => ImageAcc ix a -> Map Channel.Name (Exp a)
    -> Maybe (Mask ix a) -> Maybe Premultiply -> Exp a
    -> Image.Result (ImageAcc ix a)
multiply = math (*)

gamma :: (A.Elt a, A.IsFloating a, A.Shape ix)
    => ImageAcc ix a -> Map Channel.Name (Exp a)
    -> Maybe (Mask ix a) -> Maybe Premultiply -> Exp a
    -> Image.Result (ImageAcc ix a)
gamma = math (\v -> (**(1/v)))

-- TODO: test this comparing to nuke, there are some differences atm
-- probably the result in nuke is being calculated based on an AND|OR relation between all input channels
clamp :: (A.Shape ix, A.Elt a, A.IsFloating a)
    => ImageAcc ix a -> Map Channel.Name (Clamp (Exp a))
    -> Maybe (Mask ix a) -> Maybe Premultiply -> Exp a
    -> Image.Result (ImageAcc ix a)
clamp img ranges maskInfo premultiply mixValue = do
    unpremultiplied <- Comp.unpremultiply img premultiply
    result          <- Comp.premultiply (Image.mapWithKey handleChan unpremultiplied) premultiply
    resultMixed     <- Merge.mix' img result mixValue
    Merge.mask resultMixed img maskInfo
    where handleChan name chan = case Map.lookup name ranges of
              Nothing    -> chan
              Just value -> let (thresholds, clampTo) = value
                            in Channel.map (\x -> U.clamp thresholds clampTo x) chan

-- INFO: in Nuke this does not have the OR relation between channels, it has something pretty weird
-- soooooo....... either fuck it and do it our way or... focus on it later on
clipTest :: (A.Shape ix, A.Elt a, A.IsFloating a)
    => ImageAcc ix a -> Map Channel.Name (Range (Exp a))
    -> Maybe (Mask ix a) -> Maybe Premultiply -> Exp a
    -> Image.Result (ImageAcc ix a)
clipTest img ranges maskInfo premultiply mixValue = do
    unpremultiplied <- Comp.unpremultiply img premultiply
    result          <- Comp.premultiply (Image.mapWithKey handleChan unpremultiplied) premultiply
    resultMixed     <- Merge.mix' img result mixValue
    Merge.mask resultMixed img maskInfo
    where handleChan name chan = case Map.lookup name ranges of
              Nothing -> chan
              Just _  -> Channel.zipWith zebraStripes chan matched
          zebraStripes x b      = b A.? (1, x) -- TODO: this should make stripes, not constant color 1 ;]
          matched               = Image.foldrWithKey fold initialAcc filteredChans
          fold name chan acc    = let Just (Range lo hi) = Map.lookup name ranges -- this works because we've limited the choice to only names in the `ranges` through `fitleredChans`
                                  in Channel.zipWith (A.||*) acc (Channel.map (withinRange lo hi) chan)
          withinRange lo hi x   = x A.<* lo A.||* x A.>* hi
          initialAcc            = Channel.fill shape $ A.constant False
          shape = Channel.shape $ snd $ Image.elementAt 0 filteredChans
          filteredChans         = Image.filterByName (Map.keys ranges) img

invert :: (A.Shape ix, A.Elt a, A.IsFloating a)
    => ImageAcc ix a -> Channel.Select -> Maybe (Clamp (Exp a))
    -> Maybe (Mask ix a) -> Maybe Premultiply -> Exp a
    -> Image.Result (ImageAcc ix a)
invert img channels clampVal maskInfo premultiply mixValue = do
    unpremultiplied <- Comp.unpremultiply img premultiply
    result          <- Comp.premultiply (handleInvert unpremultiplied) premultiply
    resultMixed     <- Merge.mix' img result mixValue
    Merge.mask resultMixed img maskInfo
    where handleInvert img' = case channels of
              Channel.AllChannels      -> Image.mapChannels invertAndClamp img'
              Channel.ChannelList list -> Image.mapWithKey (invertChan list) img'
          invertChan list name chan = if name `elem` list
                                          then Channel.map invertAndClamp chan
                                          else chan
          invertAndClamp x = case clampVal of
              Nothing                         -> U.invert x
              Just (clampThresholds, clampTo) -> U.clamp clampThresholds clampTo $ U.invert $ x
