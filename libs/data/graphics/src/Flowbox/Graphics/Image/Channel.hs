---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Graphics.Image.Channel where

import Flowbox.Math.Matrix as M
import Flowbox.Prelude



type Name = String
type Select = [Name]

data Channel = ChannelFloat Name (ChannelData Double)
             | ChannelInt   Name (ChannelData Int)
             | ChannelBit   Name (ChannelData Bool)
             deriving (Show)

data ChannelData a = FlatData { _matrix     :: Matrix2 a
                              --, dataBounds :: Bounds
                              }
                              deriving (Show)
makeLenses ''ChannelData

name :: Channel -> Name
name (ChannelFloat n _) = n
name (ChannelInt   n _) = n
name (ChannelBit   n _) = n

compute :: Backend -> Channel -> Channel
compute b (ChannelFloat n (FlatData mat)) = ChannelFloat n . FlatData . M.compute b $ mat
compute b (ChannelInt   n (FlatData mat)) = ChannelInt   n . FlatData . M.compute b $ mat
compute b (ChannelBit   n (FlatData mat)) = ChannelBit   n . FlatData . M.compute b $ mat
