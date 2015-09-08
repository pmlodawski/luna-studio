---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Flowbox.Graphics.Image.Channel where

import           Data.Array.Accelerate (Boundary(..), constant)
import qualified Data.Array.Accelerate as A
import           Data.Typeable
import           Math.Space.Space (Grid(..))

import Flowbox.Graphics.Shader.Matrix
import Flowbox.Graphics.Shader.Rasterizer
import Flowbox.Graphics.Shader.Sampler
import Flowbox.Graphics.Shader.Shader
import Flowbox.Math.Matrix                as M hiding ((++))
import Flowbox.Prelude



type Name = String
type Select = [Name]

data Channel = ChannelFloat     Name (ChannelData Float) -- TODO[KM]: add a ChannelDouble constructor
             | ChannelInt       Name (ChannelData Int)

data ChannelData a = MatrixData     (Matrix2 a)
                   | DiscreteData   (DiscreteShader (Exp a))
                   | ContinuousData (ContinuousShader (Exp a)) -- TODO[KM]: figure out what to do with the space being parametrised using Double and not Float (most processing should be done with Floats as it is way faster, so what about the Double?)

instance Show Channel where
    show c = "Channel {name = \"" ++ name c ++ "\", data = " ++ dataType ++ "}"
        where dataType = case c of
                             ChannelFloat _ d -> typeOf d
                             ChannelInt   _ d -> typeOf d
              typeOf d = case d of
                             MatrixData{}     -> "Matrix2"
                             DiscreteData{}   -> "DiscreteShader"
                             ContinuousData{} -> "ContinousShader"

data Fun = FunFloat  (Exp Float -> Exp Float)
         | FunDouble (Exp Double -> Exp Double)
         | FunInt    (Exp Int -> Exp Int)

name :: Channel -> Name
name (ChannelFloat     n _) = n
name (ChannelInt       n _) = n

size :: Channel -> (Exp Int, Exp Int)
size = \case
    (ChannelFloat _ zeData) -> getSize zeData
    (ChannelInt   _ zeData) -> getSize zeData
    where getSize :: Elt a => ChannelData a -> (Exp Int, Exp Int)
          getSize = \case
              MatrixData     m -> let (A.unlift -> Z :. h :. w) = M.shape m in (w, h)
              DiscreteData   (Shader (Grid w h) _) -> (w, h)
              ContinuousData (Shader (Grid w h) _) -> (w, h)

compute :: Backend -> Sampler Double -> Channel -> Channel
compute b _ (ChannelFloat     n d) = ChannelFloat n . computeData b $ d
compute b _ (ChannelInt       n d) = ChannelInt   n . computeData b $ d

computeData :: (Elt e) => Backend -> ChannelData e -> ChannelData e
computeData b (asMatrixData -> MatrixData matrix) = MatrixData $ M.compute b matrix

asMatrix :: Channel -> Channel
asMatrix chan = case chan of
    (ChannelFloat name zeData) -> ChannelFloat name $ asMatrixData zeData
    (ChannelInt   name zeData) -> ChannelInt   name $ asMatrixData zeData

asMatrixData :: Elt e => ChannelData e -> ChannelData e
asMatrixData zeData@MatrixData{}     = zeData
asMatrixData (DiscreteData zeData)   = MatrixData $ rasterizer zeData
asMatrixData (ContinuousData zeData) = MatrixData $ (rasterizer . monosampler) zeData

asDiscrete :: Channel -> Channel
asDiscrete chan = case chan of
    (ChannelFloat name zeData) -> ChannelFloat name $ asDiscreteData (constant 0) zeData
    (ChannelInt   name zeData) -> ChannelInt   name $ asDiscreteData (constant 0) zeData

asDiscreteClamp :: Channel -> Channel
asDiscreteClamp chan = case chan of
    (ChannelFloat name zeData) -> ChannelFloat name $ asDiscreteData zeData
    (ChannelInt   name zeData) -> ChannelInt   name $ asDiscreteData zeData
    where asDiscreteData zeData@DiscreteData{}   = zeData
          asDiscreteData (MatrixData zeData)     = DiscreteData $ fromMatrix Clamp zeData
          asDiscreteData (ContinuousData zeData) = DiscreteData $ monosampler zeData

asContinuous :: Channel -> Channel
asContinuous chan = case chan of
    (ChannelFloat name zeData) -> ChannelFloat name $ asContinuousData (constant 0) zeData
    (ChannelInt   name zeData) -> ChannelInt   name $ asContinuousData (constant 0) zeData

asDiscreteData :: Elt e => Exp e -> ChannelData e -> ChannelData e
asDiscreteData _ zeData@DiscreteData{}   = zeData
asDiscreteData v (MatrixData zeData)     = DiscreteData $ fromMatrix (Constant v) zeData
asDiscreteData _ (ContinuousData zeData) = DiscreteData $ monosampler zeData

asContinuousData :: Elt e => Exp e -> ChannelData e -> ChannelData e
asContinuousData _ zeData@ContinuousData{} = zeData
asContinuousData v (MatrixData zeData)     = ContinuousData $ (nearest . fromMatrix (Constant v)) zeData
asContinuousData _ (DiscreteData zeData)   = ContinuousData $ nearest zeData

mapOverData :: Elt a => (Exp a -> Exp a) -> ChannelData a -> ChannelData a
mapOverData f chanData = case chanData of
    MatrixData mat        -> MatrixData     $ M.map f mat
    DiscreteData shader   -> DiscreteData   $ fmap f shader
    ContinuousData shader -> ContinuousData $ fmap f shader

unsafeMap :: Fun -> Channel -> Channel
unsafeMap (FunDouble f) _ = undefined -- TODO[KM]: add support for functions working on Floats after migrating to Floats and Doubles
unsafeMap (FunFloat f)  (ChannelFloat n zeData) = ChannelFloat n (mapOverData f zeData)
unsafeMap (FunInt f)    (ChannelInt n zeData)   = ChannelInt   n (mapOverData f zeData)
unsafeMap _ _ = error "Flowbox.Graphics.Image.Channel.unsafeMap - error: mismatching function type and Channel type"

