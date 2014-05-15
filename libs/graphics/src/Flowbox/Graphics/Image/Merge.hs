---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Image.Merge where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A

import           Flowbox.Graphics.Image             (Image)
import qualified Flowbox.Graphics.Image             as Image
import           Flowbox.Graphics.Image.Channel     (ChannelAcc)
import qualified Flowbox.Graphics.Image.Channel     as Channel
--import           Flowbox.Graphics.Image.Composition (Premultiply(..), Mask(..), Clamp)
import           Flowbox.Graphics.Image.Composition (Mask(..), MaskSource(..))
import qualified Flowbox.Graphics.Image.Composition as Comp
import qualified Flowbox.Graphics.Utils             as U
import           Flowbox.Prelude                    as P

data MergeOperation ix a = ATop         Channel.Name Channel.Name
                         | Average
                         | ColorBurn
                         | ColorDodge
                         | ConjointOver Channel.Name Channel.Name
                         | Copy
                         | Difference
                         | DisjointOver Channel.Name Channel.Name
                         | DivideByDst
                         | DivideBySrc
                         | Divide
                         | Exclusion
                         | From
                         | Geometric
                         | HardLight
                         | Hypot
                         | In           Channel.Name
                         | WithMask     Channel.Name
                         | Matte        Channel.Name
                         | Max
                         | Min
                         | Minus
                        -- | Mix          (Exp a) -- INFO: implemented this as a separate function
                                                  -- was identical to `Copy` with `mix` attribute set in the `merge` function)
                                                  -- made no sense to make this an option of merge, since it would be possible to set the mix-amount twice
                         | Multiply
                         | Out          Channel.Name
                         | Over         Channel.Name
                         | Overlay
                         | Plus
                         | Screen
                         | SoftLight
                         | Stencil      Channel.Name
                         | Under        Channel.Name
                         | XOR          Channel.Name Channel.Name
--               | AddMix (Map ChannelName (Curve, Curve))
--                         | Custom (ImageAcc ix a -> ImageAcc ix a -> Image.Result (ImageAcc ix a))

applyMerge :: (A.Shape ix, A.Elt a, A.IsFloating a, Image img (ChannelAcc ix a)) => MergeOperation ix a -> img (ChannelAcc ix a) -> img (ChannelAcc ix a) -> Image.Result (img (ChannelAcc ix a))
applyMerge mergeType overlay background = case mergeType of
    ATop alphaNameOv alphaNameBg -> do alphaChanOv <- Image.get alphaNameOv overlay
                                       alphaChanBg <- Image.get alphaNameBg background
                                       mergeWith4 alphaChanOv alphaChanBg $
                                           \alphaOv alphaBg ov bg -> bg * alphaOv + ov * U.invert alphaBg -- TODO: nuke version, gotta check this later and make sure it works as intended, kinda feels like imagemagick explains it a bit differently
    Average           -> mergeWith $ \ov bg -> (bg + ov) / 2
    ColorBurn         -> mergeWith $ \ov bg -> U.invert $ U.invert bg / ov
    ColorDodge        -> mergeWith $ \ov bg -> bg / U.invert ov
    ConjointOver alphaNameOv alphaNameBg -> do alphaChanOv <- Image.get alphaNameOv overlay
                                               alphaChanBg <- Image.get alphaNameBg background
                                               mergeWith4 alphaChanOv alphaChanBg $ 
                                                   \alphaOv alphaBg ov bg -> A.cond (alphaBg A.>* alphaOv) bg $ bg + ov * U.invert alphaBg / alphaOv
    Copy              -> mergeWith (\_ bg -> bg)
    Difference        -> mergeWith (\ov bg -> abs $ bg - ov)
    DisjointOver alphaNameOv alphaNameBg -> do alphaChanOv <- Image.get alphaNameOv overlay
                                               alphaChanBg <- Image.get alphaNameBg background
                                               mergeWith4 alphaChanOv alphaChanBg $
                                                   \alphaOv alphaBg ov bg -> A.cond (alphaBg + alphaOv A.<* 1) (bg + ov) $ bg + ov * U.invert alphaBg / alphaOv
    DivideByDst       -> mergeWith divideWithCheck
    DivideBySrc       -> mergeWith $ flip divideWithCheck
    Divide            -> mergeWith divideWithCheck
    Exclusion         -> mergeWith (\ov bg -> bg + ov - 2 * bg * ov)
    From              -> mergeWith (-)
    Geometric         -> mergeWith (\ov bg -> 2 * bg * ov / (bg + ov))
    HardLight         -> mergeWith $ flip overlayFun
    Hypot             -> mergeWith (\ov bg -> sqrt $ bg ^ 2 + ov ^ 2)
    In alphaName      -> do alphaChan <- Image.get alphaName overlay
                            mergeWith3 alphaChan $ \alpha _ bg -> bg * alpha
    WithMask alphaName -> do alphaChan <- Image.get alphaName background
                             mergeWith3 alphaChan $ \alpha ov _ -> ov * alpha
    Matte alphaName   -> do alphaChan <- Image.get alphaName background
                            mergeWith3 alphaChan $ \alpha ov bg -> bg * alpha + ov * U.invert alpha
    Max               -> mergeWith max
    Min               -> mergeWith min
    Minus             -> mergeWith $ flip (-)
    --Mix val           -> mergeWith (\ov bg -> bg * val + ov * (U.invert val)) -- INFO: commented out because: check the datatype
    Multiply          -> mergeWith multiplyWithCheck
    Out alphaName     -> do alphaChan <- Image.get alphaName overlay
                            mergeWith3 alphaChan $ \alpha _ bg -> bg * U.invert alpha
    Over alphaName    -> do alphaChan <- Image.get alphaName background
                            mergeWith3 alphaChan $ \alpha ov bg -> bg + ov * U.invert alpha
    Overlay           -> mergeWith overlayFun
    Plus              -> mergeWith (+)
    Screen            -> mergeWith $ \ov bg -> U.invert $ U.invert ov * U.invert bg -- INFO: imagemagick version, nuke does this in a slightly different way but the result might be the same
    SoftLight         -> mergeWith softLightFunc
    Stencil alphaName -> do alphaChan <- Image.get alphaName background
                            mergeWith3 alphaChan $ \alpha ov _ -> ov * U.invert alpha
    Under alphaName   -> do alphaChan <- Image.get alphaName overlay
                            mergeWith3 alphaChan $ \alpha ov bg -> bg * U.invert alpha + ov
    XOR alphaNameOv alphaNameBg -> do alphaChanOv <- Image.get alphaNameOv overlay
                                      alphaChanBg <- Image.get alphaNameBg background
                                      mergeWith4 alphaChanOv alphaChanBg $
                                          \alphaOv alphaBg ov bg -> bg * U.invert alphaOv + ov * U.invert alphaBg
--  AddMix (Map ChannelName (Curve, Curve))
--    Custom f          -> f overlay background
    where mergeWith f              = return $ Image.channelUnionWith (Channel.zipWith f)              overlay background
          mergeWith3 chan f        = return $ Image.channelUnionWith (Channel.zipWith3 f chan)        overlay background
          mergeWith4 chanA chanB f = return $ Image.channelUnionWith (Channel.zipWith4 f chanA chanB) overlay background
          divideWithCheck a b      = checkForNegatives a b (a / b)
          multiplyWithCheck a b    = checkForNegatives a b (a * b)
          checkForNegatives a b    = A.cond (a A.<* 0 A.&&* b A.<* 0) 0
          overlayFun ov bg         = A.cond (bg A.<=* 0.5) (2 * bg * ov) (1 - 2 * (1 - ov) * (1 - bg))
          softLightFunc ov bg      = A.cond (bg A.<=* 0.5) (2 * bg * (ov / 2 + 0.25)) (U.invert $ 2 * U.invert bg * U.invert (ov / 2 + 0.25)) -- INFO: photoshop (?) version, nuke does this in a slightly different way and might produce different results


mask :: (A.Shape ix, A.Elt a, A.IsNum a, Image img (ChannelAcc ix a)) => img (ChannelAcc ix a) -> img (ChannelAcc ix a) -> Maybe (Mask img ix a) -> Image.Result (img (ChannelAcc ix a))
mask imgA _ Nothing = Right imgA
mask imgA imgB (Just theMask) = do
    maskChan <- Image.get name maskImg
    let applyMask = Channel.zipWith3 calculateMask maskChan
        calculateMask alpha = flip (U.mix (Comp.invert invertFlag alpha))
    return $ Comp.inject injection maskChan
           $ Image.channelUnion (Image.channelIntersectionWith applyMask imgA imgB) imgA
    where Mask name injection invertFlag maskImg' = theMask --case theMask of
          maskImg = case maskImg' of
              Local        -> imgA
              External img -> img
              --ChannelMask name' injection' invertFlag'          -> (name', injection', invertFlag', imgA)
              --ImageMask   name' injection' invertFlag' maskImg' -> (name', injection', invertFlag', maskImg')


mix :: (A.Shape ix, A.Elt a, A.IsFloating a, Image img (ChannelAcc ix a))
    => img (ChannelAcc ix a) -> img (ChannelAcc ix a) -> Exp a
    -> Maybe (Mask img ix a)
    -> Image.Result (img (ChannelAcc ix a))
mix imgA imgB val maskInfo = do
    let result = Image.channelUnion (Image.channelIntersectionWith (Channel.zipWith (U.mix val)) imgA imgB) imgA
    mask result imgA maskInfo

-- INFO: `mix` but without using a mask (quite an often scenario, since the functions using `mix` apply the mask on their own)
mix' :: (A.Shape ix, A.Elt a, A.IsFloating a, Image img (ChannelAcc ix a))
    => img (ChannelAcc ix a) -> img (ChannelAcc ix a) -> Exp a -> Image.Result (img (ChannelAcc ix a))
mix' imgA imgB val = mix imgA imgB val Nothing


-- TODO: add alpha masking, bounding boxes, metadata, etc. (check confluence)
-- TODO: should there be a third Channel.Select for outgoing channels?
--       same thing can be achieved by not selecting channels from the image we're merging with
merge :: (A.Shape ix, A.Elt a, A.IsFloating a, Image img (ChannelAcc ix a))
    => img (ChannelAcc ix a) -> img (ChannelAcc ix a) -> MergeOperation ix a -> Channel.Select -> Channel.Select
    -> Maybe (Mask img ix a) -> Exp a
    -> Image.Result (img (ChannelAcc ix a))
merge overlay background mergeType channelsOv channelsBg maskInfo mixValue = do
    result      <- applyMerge mergeType overlay' background'
    resultMixed <- mix' overlay result mixValue
    mask resultMixed background maskInfo
    where overlay'    = Image.selectChannels channelsOv overlay
          background' = Image.selectChannels channelsBg background
