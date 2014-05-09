---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.Graphics.ImageRGBA where

import           Data.Map (Map)
--import qualified Data.Map as Map

import           Flowbox.Graphics.Image
import           Flowbox.Graphics.Image.Channel (Channel)
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Prelude                hiding (lookup, map)



newtype ImageRGBA a = ImageRGBA { _channels' :: Map Channel.Name a } deriving (Monoid, Show)
makeLenses ''ImageRGBA

instance Functor ImageRGBA where
    fmap f (ImageRGBA channelMap) = ImageRGBA $ fmap f channelMap

instance Image ImageRGBA a where
    channels = channels'

--instance Monoid (ImageRGBA a) where
--    mempty        = ImageRGBA mempty
--    a `mappend` b = ImageRGBA $ (view channels' a) `mappend` (view channels' b)
