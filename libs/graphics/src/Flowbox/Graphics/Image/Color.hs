---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}

module Flowbox.Graphics.Image.Color where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A
import           Data.Map              (Map)
import qualified Data.Map              as Map

--import qualified Debug.Trace as Dbg

--import           Diagrams.Prelude                (R2, Diagram)
--import           Diagrams.Backend.Cairo.Internal (Cairo)

--import qualified Flowbox.Graphics.Color             as Color
import           Flowbox.Graphics.Image             (Image)
import qualified Flowbox.Graphics.Image             as Image
import           Flowbox.Graphics.Image.Channel     (ChannelAcc, Channel2)
import qualified Flowbox.Graphics.Image.Channel     as Channel
import           Flowbox.Graphics.Image.Composition (Premultiply(..), Mask(..), Clamp)
import qualified Flowbox.Graphics.Image.Composition as Comp
import qualified Flowbox.Graphics.Image.Merge       as Merge
import           Flowbox.Graphics.Utils             (Range(..))
import qualified Flowbox.Graphics.Utils             as U
import           Flowbox.Prelude                    as P


-- TODO: discuss:
-- img = open "ala.png"
-- img.rgb.r
-- should those functions encapsulate the Image type in something holding the info about the color space, so u can use the line above?


math :: (A.Elt a, A.IsFloating a, A.Shape ix, Image img (ChannelAcc ix a))
    => (Exp a -> Exp a -> Exp a) -> img (ChannelAcc ix a) -> Map Channel.Name (Exp a)
    -> Maybe (Mask img ix a) -> Maybe Premultiply -> Exp a -- TODO: add to wiki why this line is separated from others
    -> Image.Result (img (ChannelAcc ix a))
math f img values maskInfo premultInfo mixValue = do
    unpremultiplied <- Comp.unpremultiply img premultInfo
    let result      =  Image.mapWithKey handleChan unpremultiplied
    premultiplied   <- Comp.premultiply result  premultInfo
    resultMixed     <- Merge.mix' img premultiplied mixValue
    Merge.mask resultMixed img maskInfo
    where handleChan name chan = case Map.lookup name values of
              Nothing    -> chan
              Just value -> Channel.map (f value) chan

offset :: (A.Elt a, A.IsFloating a, A.Shape ix, Image img (ChannelAcc ix a))
    => img (ChannelAcc ix a) -> Map Channel.Name (Exp a)
    -> Maybe (Mask img ix a) -> Maybe Premultiply -> Exp a
    -> Image.Result (img (ChannelAcc ix a))
offset = math (+)

multiply :: (A.Elt a, A.IsFloating a, A.Shape ix, Image img (ChannelAcc ix a))
    => img (ChannelAcc ix a) -> Map Channel.Name (Exp a)
    -> Maybe (Mask img ix a) -> Maybe Premultiply -> Exp a
    -> Image.Result (img (ChannelAcc ix a))
multiply = math (*)

contrast :: (A.Shape ix, A.Elt a, A.IsFloating a, Image img (ChannelAcc ix a))
    => img (ChannelAcc ix a) -> Map Channel.Name (Exp a)
    -> Maybe (Mask img ix a) -> Maybe Premultiply -> Exp a
    -> Image.Result (img (ChannelAcc ix a))
contrast = math (\v x -> (x - 0.5) * v + 0.5)

gamma :: (A.Elt a, A.IsFloating a, A.Shape ix, Image img (ChannelAcc ix a))
    => img (ChannelAcc ix a) -> Map Channel.Name (Exp a)
    -> Maybe (Mask img ix a) -> Maybe Premultiply -> Exp a
    -> Image.Result (img (ChannelAcc ix a))
gamma = math (\v -> (**(1/v)))

-- TODO: test this comparing to nuke, there are some differences atm
-- probably the result in nuke is being calculated based on an AND|OR relation between all input channels
clamp :: (A.Shape ix, A.Elt a, A.IsFloating a, Image img (ChannelAcc ix a))
    => img (ChannelAcc ix a) -> Map Channel.Name (Clamp (Exp a))
    -> Maybe (Mask img ix a) -> Maybe Premultiply -> Exp a
    -> Image.Result (img (ChannelAcc ix a))
clamp img ranges maskInfo premultInfo mixValue = do
    unpremultiplied <- Comp.unpremultiply img premultInfo
    let result      =  Image.mapWithKey handleChan unpremultiplied
    premultiplied   <- Comp.premultiply result premultInfo
    resultMixed     <- Merge.mix' img premultiplied mixValue
    Merge.mask resultMixed img maskInfo
    where handleChan name chan = case Map.lookup name ranges of
              Nothing    -> chan
              Just value -> let (thresholds, clampTo) = value
                            in Channel.map (U.clamp thresholds clampTo) chan

-- INFO: in Nuke this does not have the OR relation between channels, it has something pretty weird
-- soooooo....... either fuck it and do it our way or... focus on it later on
-- TODO: this might have to be rewritten to avoid using filterByName
clipTest :: (A.Shape ix, A.Elt a, A.IsFloating a, Image img (ChannelAcc ix a))
    => img (ChannelAcc ix a) -> Map Channel.Name (Range (Exp a))
    -> Maybe (Mask img ix a) -> Maybe Premultiply -> Exp a
    -> Image.Result (img (ChannelAcc ix a))
clipTest img ranges maskInfo premultInfo mixValue = do
    unpremultiplied <- Comp.unpremultiply img premultInfo
    let result      =  Image.mapWithKey handleChan unpremultiplied
    premultiplied   <- Comp.premultiply result premultInfo
    resultMixed     <- Merge.mix' img premultiplied mixValue
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

invert :: (A.Shape ix, A.Elt a, A.IsFloating a, Image img (ChannelAcc ix a))
    => img (ChannelAcc ix a) -> Channel.Select -> Maybe (Clamp (Exp a))
    -> Maybe (Mask img ix a) -> Maybe Premultiply -> Exp a
    -> Image.Result (img (ChannelAcc ix a))
invert img channels clampVal maskInfo premultInfo mixValue = do
    unpremultiplied <- Comp.unpremultiply img premultInfo
    let result      =  handleInvert unpremultiplied
    premultiplied   <- Comp.premultiply result premultInfo
    resultMixed     <- Merge.mix' img premultiplied mixValue
    Merge.mask resultMixed img maskInfo
    where handleInvert img' = case channels of
              Channel.AllChannels      -> Image.mapChannels invertAndClamp img'
              Channel.ChannelList list -> Image.mapWithKey (invertChan list) img'
          invertChan list name chan = if name `elem` list
                                          then Channel.map invertAndClamp chan
                                          else chan
          invertAndClamp x = case clampVal of
              Nothing                         -> U.invert x
              Just (clampThresholds, clampTo) -> U.clamp clampThresholds clampTo $ U.invert x

-- TODO: should it really take a matrix as an input? it doesn't really speed up anything or anything
--       and can only make things harder to debug (channel names are separated from the actual values used to calculate everything)
--       I would strongly suggest using some kind of a data type for channelsOut, channelsIn and values to be put together
-- INFO: for now this only works for 2D images since I'm using lift2Dto3D
colorMatrix :: (A.Elt a, A.IsFloating a, Image img (Channel2 a))
    => img (Channel2 a) -> [Channel.Name] -> [Channel.Name] -> A.Acc (A.Array A.DIM2 a) -- -> Exp Bool -- for inverting the matrix itself (?)
    -> Maybe (Mask img A.DIM2 a) -> Maybe Premultiply -> Exp a
    -> Image.Result (img (Channel2 a))
colorMatrix img channelsOut channelsIn values maskInfo premultInfo mixValue = do
  unpremultiplied <- Comp.unpremultiply img premultInfo
  channelsIn'     <- glue <$> Image.elemsByName' channelsIn unpremultiplied
  let result      =  handleColorMatrix unpremultiplied channelsIn'
  premultiplied   <- Comp.premultiply result premultInfo
  resultMixed     <- Merge.mix' img premultiplied mixValue
  Merge.mask resultMixed img maskInfo
  where handleColorMatrix img' channelsIn' = Image.channelUnion (Image.fromList img' $ calculateChannels channelsIn') img'
  -- INFO: probably faster than the above line but less readable
  --where handleColorMatrix img' channelsIn' = foldr insertChan img' (calculateChannels img' channelsIn')
        --insertChan chanInfo imgAcc = Image.insert (fst chanInfo) (snd chanInfo) imgAcc
        calculateChannels channelsIn' = zipWith (calculateChannel channelsIn') channelsOut (iterate succ (0 :: Int))
        calculateChannel  channelsIn' name idx = (name, makeChannel channelsIn' idx)
        makeChannel channelsIn' idx = let
                (A.Z A.:. h A.:. w A.:. _) = A.unlift $ Channel.shape channelsIn' :: A.Z A.:. Exp Int A.:. Exp Int A.:. Exp Int
                values' = A.slice values $ A.lift (A.Z A.:. idx A.:. A.All)
                repl    = Channel.Acc $ A.replicate (A.lift $ A.Z A.:. h A.:. w A.:. A.All) values'
            in Channel.fold1 (+) $ Channel.zipWith (*) channelsIn' repl
        glue channels = foldr1 (Channel.++) (fmap Channel.lift2Dto3D channels)

-- TODO: rewrite this to filter out the required channels into images, then map over those images and union the result with the target image
--       instead of doing all the calculations separately for all channels
-- FIXME: this doesn't seem to generate the desired results =/
colorTransfer :: (A.Shape ix, A.Shape jx, A.Elt a, A.IsFloating a, Image img (ChannelAcc ix a), Image img (ChannelAcc jx a))
    => img (ChannelAcc ix a) -> img (ChannelAcc jx a) -- -> Diagram Cairo R2 -- TODO: use a shape to define a ROI
    -> Maybe (Mask img ix a) -> Maybe Premultiply -> Exp a -- do we need this part or not? afair nuke doesn't have those, but why shouldn't we?
    -> Image.Result (img (ChannelAcc ix a))
colorTransfer target source maskInfo premultInfo mixValue = do
    unpremultiplied <- Comp.unpremultiply target premultInfo

    targetR         <- Image.get "rgba.r" unpremultiplied
    targetG         <- Image.get "rgba.g" unpremultiplied
    targetB         <- Image.get "rgba.b" unpremultiplied
    sourceR         <- Image.get "rgba.r" source
    sourceG         <- Image.get "rgba.g" source
    sourceB         <- Image.get "rgba.b" source

    let targetL = Channel.map log $ Channel.zipWith3 calculateL targetR targetG targetB
        targetM = Channel.map log $ Channel.zipWith3 calculateM targetR targetG targetB
        targetS = Channel.map log $ Channel.zipWith3 calculateS targetR targetG targetB
        sourceL = Channel.map log $ Channel.zipWith3 calculateL sourceR sourceG sourceB
        sourceM = Channel.map log $ Channel.zipWith3 calculateM sourceR sourceG sourceB
        sourceS = Channel.map log $ Channel.zipWith3 calculateS sourceR sourceG sourceB
        targetl = Channel.zipWith3 calculatel targetL targetM targetS
        targeta = Channel.zipWith3 calculatea targetL targetM targetS
        targetb = Channel.zipWith  calculateb targetL targetM
        sourcel = Channel.zipWith3 calculatel sourceL sourceM sourceS
        sourcea = Channel.zipWith3 calculatea sourceL sourceM sourceS
        sourceb = Channel.zipWith  calculateb sourceL sourceM
        finall  = calculate targetl sourcel
        finala  = calculate targeta sourcea
        finalb  = calculate targetb sourceb
        finalL  = Channel.map exp $ Channel.zipWith3 calculateL' finall finala finalb
        finalM  = Channel.map exp $ Channel.zipWith3 calculateM' finall finala finalb
        finalS  = Channel.map exp $ Channel.zipWith  calculateS' finall finala
        target' = Image.insert "rgba.r" (Channel.zipWith3 calculateR' finalL finalM finalS)
                $ Image.insert "rgba.g" (Channel.zipWith3 calculateG' finalL finalM finalS)
                $ Image.insert "rgba.b" (Channel.zipWith3 calculateB' finalL finalM finalS)
                unpremultiplied

    result          <- Comp.premultiply target' premultInfo
    resultMixed     <- Merge.mix' target result mixValue
    Merge.mask resultMixed target maskInfo

    where calculate target' source' = Channel.map calculations target'
              where calculations x = (x - meanDev target') * ratio + meanDev source'
                    ratio = standardDev target' / standardDev source'
          meanDev chan = Channel.the (Channel.foldAll (+) 0 $ Channel.map (\x -> abs $ x - mean chan) chan) / A.fromIntegral (Channel.size chan)
          standardDev chan = sqrt $ Channel.the (Channel.foldAll (+) 0 $ Channel.map (\x -> (x - mean chan) ** 2) chan) / A.fromIntegral (Channel.size chan)
          mean chan = Channel.the (Channel.foldAll (+) 0 chan) / A.fromIntegral (Channel.size chan)
          --
          calculateL r g b  = 0.3811 * r + 0.5783 * g + 0.0402 * b
          calculateM r g b  = 0.1967 * r + 0.7244 * g + 0.0782 * b
          calculateS r g b  = 0.0241 * r + 0.1288 * g + 0.8444 * b
          --
          calculatel l m s  = (1 / sqrt 3) * l + (1 / sqrt 3) * m + (1 / sqrt 3) * s
          calculatea l m s  = (1 / sqrt 6) * l + (1 / sqrt 6) * m - (2 / sqrt 6) * s
          calculateb l m    = (1 / sqrt 2) * l - (1 / sqrt 2) * m
          --
          calculateL' l a b = (sqrt 3 / 3) * l +   (sqrt 6 / 6) * a + (sqrt 2 / 2) * b
          calculateM' l a b = (sqrt 3 / 3) * l +   (sqrt 6 / 6) * a - (sqrt 2 / 2) * b
          calculateS' l a   = (sqrt 3 / 3) * l - (2*sqrt 6 / 6) * a
          --
          calculateR' l m s =   4.4679  * l - 3.5873 * m + 0.1193 * s
          calculateG' l m s = (-1.2186) * l + 2.3809 * m - 0.1624 * s
          calculateB' l m s =   0.0497  * l - 0.2439 * m + 1.2045 * s
