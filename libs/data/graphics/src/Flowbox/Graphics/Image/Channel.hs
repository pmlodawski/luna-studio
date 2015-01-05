---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE ViewPatterns  #-}

module Flowbox.Graphics.Image.Channel where

import Flowbox.Graphics.Shader.Matrix
import Flowbox.Graphics.Shader.Rasterizer
import Flowbox.Graphics.Shader.Sampler
import Flowbox.Graphics.Shader.Shader
import Flowbox.Math.Matrix                as M hiding ((++))
import Flowbox.Prelude



type Name = String
type Select = [Name]

data Channel = ChannelFloat     Name (ChannelData Double) -- TODO[KM]: add a ChannelDouble constructor
             | ChannelInt       Name (ChannelData Int)
             | ChannelBit       Name (ChannelData Bool)
             -- | ChannelShader    Name (ContinuousShader    (Exp Double))

data ChannelData a = MatrixData     (Matrix2 a)
                   | DiscreteData   (DiscreteShader (Exp a))
                   | ContinuousData (ContinuousShader (Exp a)) -- TODO[KM]: figure out what to do with the space being parametrised using Double and not Float (most processing should be done with Floats as it is way faster, so what about the Double?)

--makeLenses ''ChannelData

instance Show Channel where
    show c = "Channel {name = \"" ++ name c ++ "\", data = " ++ dataType ++ "}"
        where dataType = case c of
                             ChannelFloat _ d -> typeOf d
                             ChannelInt   _ d -> typeOf d
                             ChannelBit   _ d -> typeOf d
              typeOf d = case d of
                             MatrixData{}     -> "Matrix2"
                             DiscreteData{}   -> "DiscreteShader"
                             ContinuousData{} -> "ContinousShader"


name :: Channel -> Name
name (ChannelFloat     n _) = n
name (ChannelInt       n _) = n
name (ChannelBit       n _) = n

compute :: Backend -> Sampler Double -> Channel -> Channel
compute b _ (ChannelFloat     n d) = ChannelFloat n . computeData b $ d
compute b _ (ChannelInt       n d) = ChannelInt   n . computeData b $ d
compute b _ (ChannelBit       n d) = ChannelBit   n . computeData b $ d

computeData :: (Elt e) => Backend -> ChannelData e -> ChannelData e
computeData b (asMatrix -> MatrixData matrix) = MatrixData $ M.compute b matrix

asMatrix :: (Elt e) => ChannelData e -> ChannelData e
asMatrix zeData@MatrixData{}     = zeData
asMatrix (DiscreteData zeData)   = MatrixData $ rasterizer zeData
asMatrix (ContinuousData zeData) = MatrixData $ (rasterizer . monosampler) zeData

asDiscrete :: (Elt e) => ChannelData e -> ChannelData e
asDiscrete zeData@DiscreteData{}   = zeData
asDiscrete (MatrixData zeData)     = DiscreteData $ unsafeFromMatrix zeData
asDiscrete (ContinuousData zeData) = DiscreteData $ monosampler zeData

asContinuous :: (Elt e) => ChannelData e -> ChannelData e
asContinuous zeData@ContinuousData{} = zeData
asContinuous (MatrixData zeData)     = ContinuousData $ (nearest . unsafeFromMatrix) zeData
asContinuous (DiscreteData zeData)   = ContinuousData $ nearest zeData
