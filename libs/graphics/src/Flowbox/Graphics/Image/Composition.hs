---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Image.Composition where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A

--import qualified Debug.Trace as Dbg

import           Flowbox.Graphics.Image         (ImageAcc)
import qualified Flowbox.Graphics.Image         as Image
import           Flowbox.Graphics.Image.Channel (ChannelAcc)
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Graphics.Utils         (Range(..))
import qualified Flowbox.Graphics.Utils         as U
import           Flowbox.Prelude                as P



data Premultiply = Premultiply   { premultChanName :: Channel.Name
                                 , invertPremult   :: Exp Bool
                                 } deriving (Show)

type Inject = Maybe Channel.Name

data Mask ix a = ChannelMask { maskChanName :: Channel.Name, injectMask :: Inject, invertMask :: Exp Bool }
               | ImageMask   { maskChanName :: Channel.Name, injectMask :: Inject, invertMask :: Exp Bool, maskImage :: ImageAcc ix a }
               deriving (Show)

type Clamp a = (Range a, Maybe (Range a))

invert :: (A.Elt a, A.IsNum a) => Exp Bool -> Exp a -> Exp a
invert p x = p A.? (U.invert x, x)

inject :: Inject -> ChannelAcc ix a -> ImageAcc ix a -> ImageAcc ix a
inject injection chan img = case injection of
    Nothing   -> img
    Just name -> Image.insert name chan img

premultiply :: (A.Elt a, A.IsFloating a, A.Shape ix) => ImageAcc ix a -> Maybe Premultiply -> Image.Result (ImageAcc ix a)
premultiply img Nothing = Right img
premultiply img (Just (Premultiply name invertFlag)) = do
    alphaChan <- Image.get name img
    return $ Image.map (Channel.zipWith (\a x -> x * (invert invertFlag a)) alphaChan) img

unpremultiply :: (A.Elt a, A.IsFloating a, A.Shape ix) => ImageAcc ix a -> Maybe Premultiply -> Image.Result (ImageAcc ix a)
unpremultiply img Nothing = Right img
unpremultiply img (Just (Premultiply name invertFlag)) = do
    alphaChan <- Image.get name img
    return $ Image.map (Channel.zipWith (\a x -> x / (invert invertFlag a)) alphaChan) img
