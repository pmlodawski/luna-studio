---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Image.Composition where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A

import           Flowbox.Graphics.Image         (ImageAcc)
import qualified Flowbox.Graphics.Image         as Image
import           Flowbox.Graphics.Image.Channel (ChannelAcc)
import qualified Flowbox.Graphics.Image.Channel as Channel
import qualified Flowbox.Graphics.Utils         as U
import           Flowbox.Prelude                as P

data Premultiply = Premultiply   { premultChanName :: Channel.Name, invertPremult :: Exp Bool }
                 | Unpremultiply { premultChanName :: Channel.Name, invertPremult :: Exp Bool }

type Inject = Maybe Channel.Name

data Mask ix a = ChannelMask { maskChanName :: Channel.Name, injectMask :: Inject, invertMask :: Exp Bool }
               | ImageMask   { maskChanName :: Channel.Name, injectMask :: Inject, invertMask :: Exp Bool, maskImage :: ImageAcc ix a }

mix :: (A.Elt a, A.IsNum a) => Exp a -> Exp a -> Exp a -> Exp a
mix value oldValue newValue = (U.invert value) * oldValue + value * newValue

invert :: (A.Elt a, A.IsNum a) => Exp Bool -> Exp a -> Exp a
invert p x = p A.? (U.invert x, x)

inject :: Inject -> ChannelAcc ix a -> ImageAcc ix a -> ImageAcc ix a
inject injection chan img = case injection of
    Nothing   -> img
    Just name -> Image.insert name chan img

combineWithMask :: (A.Shape ix, A.Elt a, A.IsNum a) => ImageAcc ix a -> ImageAcc ix a -> Maybe (Mask ix a) -> Either Image.Error (ImageAcc ix a)
combineWithMask imgA _ Nothing = Right imgA
combineWithMask imgA imgB (Just theMask) = case theMask of
    ChannelMask name injection invertFlag -> do
        maskChan  <- Image.get name imgA
        return $ handleMask maskChan injection invertFlag
    ImageMask name injection invertFlag maskImg -> do
        maskChan <- Image.get name maskImg
        return $ handleMask maskChan injection invertFlag
    where handleMask maskChan injection invertFlag = inject injection maskChan
                                                   $ Image.channelIntersectionWith (applyMask invertFlag maskChan) imgA imgB
          applyMask invertFlag maskChan chanA chanB = Channel.zipWith3 (calculateMask invertFlag) maskChan chanA chanB
          calculateMask i m a b = A.cond (m A.>* 0) (i A.? (b, a)) (i A.? (a, b))
