---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Composition.Internal where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A

import           Flowbox.Graphics.Channel (ChannelAcc)
import qualified Flowbox.Graphics.Channel as Channel
import           Flowbox.Graphics.Image   (Image)
import qualified Flowbox.Graphics.Image   as Image
import           Flowbox.Graphics.Utils   (Range(..))
import qualified Flowbox.Graphics.Utils   as U
import           Flowbox.Prelude          as P



data Premultiply = Premultiply { premultChanName :: Channel.Name
                               , invertPremult   :: Exp Bool
                               } deriving (Show)

type Inject = Maybe Channel.Name

data Mask img ix a  = Mask { maskChanName :: Channel.Name
                           , injectMask   :: Inject
                           , invertMask   :: Exp Bool
                           , sourceImg    :: MaskSource img ix a
                           }

data MaskSource img ix a = Local
                         | External (img (ChannelAcc ix a))

type Clamp a = (Range a, Maybe (Range a))

invert :: (A.Elt a, A.IsNum a) => Exp Bool -> Exp a -> Exp a
invert p x = p A.? (U.invert x, x)

inject :: Image img (ChannelAcc ix a)
    => Inject -> ChannelAcc ix a -> img (ChannelAcc ix a) -> img (ChannelAcc ix a)
inject injection chan img = case injection of
    Nothing   -> img
    Just name -> Image.insert name chan img

premultiply :: (A.Elt a, A.IsFloating a, A.Shape ix, Image img (ChannelAcc ix a))
    => img (ChannelAcc ix a) -> Maybe Premultiply -> Image.Result (img (ChannelAcc ix a))
premultiply img Nothing = Right img
premultiply img (Just (Premultiply name invertFlag)) = do
    alphaChan <- Image.get name img
    return $ Image.map (Channel.zipWith (\a x -> x * invert invertFlag a) alphaChan) img

unpremultiply :: (A.Elt a, A.IsFloating a, A.Shape ix, Image img (ChannelAcc ix a))
    => img (ChannelAcc ix a) -> Maybe Premultiply -> Image.Result (img (ChannelAcc ix a))
unpremultiply img Nothing = Right img
unpremultiply img (Just (Premultiply name invertFlag)) = do
    alphaChan <- Image.get name img
    return $ Image.map (Channel.zipWith (\a x -> x / invert invertFlag a) alphaChan) img
