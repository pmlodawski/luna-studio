---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.Graphics.Image.ImageRGB where

import           Data.Map (Map)

import           Flowbox.Graphics.Image
import			 Flowbox.Graphics.Image.Channel (ChannelAcc)
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Prelude                hiding (lookup, map)



newtype ImageRGB a = ImageRGB { _channels' :: Map Channel.Name a } deriving (Monoid, Functor, Show)
makeLenses ''ImageRGB

instance Image ImageRGB a where
    channels = channels'

type ImageAccRGB ix a = ImageRGB (ChannelAcc ix a)
