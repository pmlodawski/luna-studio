---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Image.Channel where

import Data.Array.Accelerate (DIM2)

import Flowbox.Math.Matrix
import Flowbox.Prelude



type Name = String

data Channel = ChannelFloat (ChannelData Double)
             | ChannelInt   (ChannelData Int)
             | ChannelBit   (ChannelData Bool)
             deriving (Show)

data ChannelData a = FlatData { matrix     :: Matrix DIM2 a
                              --, dataBounds :: Bounds
                              }
                              deriving (Show)
