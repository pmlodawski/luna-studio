---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.Graphics.Image.Color.YUV_HD where

import           Data.Map (Map)

import           Flowbox.Graphics.Image
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Prelude                hiding (lookup, map)



newtype ImageYUV_HD a = ImageYUV_HD { _channels' :: Map Channel.Name a } deriving (Monoid, Functor, Show)
makeLenses ''ImageYUV_HD

instance Image ImageYUV_HD a where
    channels = channels'
