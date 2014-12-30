---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

module Flowbox.Graphics.Image.Channel where

import Flowbox.Graphics.Shader.Rasterizer
import Flowbox.Graphics.Shader.Sampler
import Flowbox.Graphics.Shader.Shader
import Flowbox.Math.Matrix 				  as M hiding ((++))
import Flowbox.Prelude



type Name = String
type Select = [Name]

data Channel = ChannelFloat     Name (ChannelData Double)
             | ChannelInt       Name (ChannelData Int)
             | ChannelBit       Name (ChannelData Bool)
             | ChannelGenerator Name (ContinousGenerator (Exp Double))

data ChannelData a = FlatData { _matrix :: Matrix2 a }
                   deriving Show

makeLenses ''ChannelData

instance Show Channel where
    show c = "Channel {name = \"" ++ name c ++ "\"}"

name :: Channel -> Name
name (ChannelFloat     n _) = n
name (ChannelInt       n _) = n
name (ChannelBit       n _) = n
name (ChannelGenerator n _) = n

compute :: Backend -> Sampler Double -> Channel -> Channel
compute b _ (ChannelFloat     n d) = ChannelFloat n . computeFlatData b $ d
compute b _ (ChannelInt       n d) = ChannelInt   n . computeFlatData b $ d
compute b _ (ChannelBit       n d) = ChannelBit   n . computeFlatData b $ d
compute b s (ChannelGenerator n g) = ChannelFloat n . FlatData . rasterizer . s $ g

computeFlatData :: (Elt e) => Backend -> ChannelData e -> ChannelData e
computeFlatData b = over matrix $ M.compute b
