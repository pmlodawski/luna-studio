---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternGuards #-}

module Flowbox.Graphics.Image.Color where

import qualified Data.Array.Accelerate as A
import           Data.Maybe            (fromMaybe)

import           Flowbox.Graphics.Composition.Generators.Structures (Generator(..))
import           Flowbox.Graphics.Image.Channel                     (Channel)
import qualified Flowbox.Graphics.Image.Channel                     as Channel
import qualified Flowbox.Graphics.Utils                             as U
import qualified Flowbox.Math.Matrix                                as Matrix
import           Flowbox.Prelude



mapInt :: (A.Exp Int -> A.Exp Int) -> Channel -> Channel
mapInt f chan = case chan of
    Channel.ChannelInt name (Channel.FlatData m) -> Channel.ChannelInt name $ Channel.FlatData $ Matrix.map f m
    _ -> error "Function from Int to Int could not be applied to this type of Channel!"

mapFloat :: (A.Exp Double -> A.Exp Double) -> Channel -> Channel
mapFloat f chan = case chan of
    Channel.ChannelFloat name (Channel.FlatData m) -> Channel.ChannelFloat name $ Channel.FlatData $ Matrix.map f m
    _ -> error "Function from Double to Double could not be applied to this type of Channel!"

mapBool :: (A.Exp Bool -> A.Exp Bool) -> Channel -> Channel
mapBool f chan = case chan of
    Channel.ChannelBit name (Channel.FlatData m) -> Channel.ChannelBit name $ Channel.FlatData $ Matrix.map f m
    _ -> error "Function from Bool to Bool could not be applied to this type of Channel!"

offset :: A.Exp Double -> Channel -> Channel
offset v chan | Channel.ChannelInt{}   <- chan = mapInt (+ (A.truncate v)) chan
              | Channel.ChannelFloat{} <- chan = mapFloat (+ v) chan
              | otherwise                      = crash "offset" "Bool"

offset' :: A.Exp Double -> Generator -> Generator
offset' v gen = Generator $ \p s -> runGenerator gen p s + v

multiply :: A.Exp Double -> Channel -> Channel
multiply v chan | Channel.ChannelInt{}   <- chan = mapInt (* (A.truncate v)) chan
                | Channel.ChannelFloat{} <- chan = mapFloat (* v) chan
                | otherwise                      = crash "multiply" "Bool"

multiply' :: A.Exp Double -> Generator -> Generator
multiply' v gen = Generator $ \p s -> runGenerator gen p s * v

not :: Channel -> Channel
not chan | Channel.ChannelBit{} <- chan = mapBool (A.not) chan
         | otherwise                    = crash "not" "Numeric"

contrast :: A.Exp Double -> Channel -> Channel
contrast v chan | Channel.ChannelInt{}   <- chan = mapInt (\x -> A.truncate $ (A.fromIntegral x - 0.5) * v + 0.5) chan
                | Channel.ChannelFloat{} <- chan = mapFloat (\x -> (x - 0.5) * v + 0.5) chan
                | otherwise                      = crash "contrast" "Bool"

contrast' :: A.Exp Double -> Generator -> Generator
contrast' v gen = Generator $ \p s -> (runGenerator gen p s - 0.5) * v + 0.5

gamma :: A.Exp Double -> Channel -> Channel
gamma v chan | Channel.ChannelInt{}   <- chan = mapInt (\x -> A.truncate $ A.fromIntegral x ** (1/v)) chan
             | Channel.ChannelFloat{} <- chan = mapFloat (** (1/v)) chan
             | otherwise                      = crash "gamma" "Bool"

gamma' :: A.Exp Double -> Generator -> Generator
gamma' v gen = Generator $ \p s -> runGenerator gen p s ** (1/v)

clamp :: A.Exp Double -> A.Exp Double -> Maybe (A.Exp Double) -> Maybe (A.Exp Double) -> Generator -> Generator
clamp min' max' minClampTo maxClampTo gen = Generator $ \p s -> 
    let pixel = runGenerator gen p s
    in (pixel A.<* min') A.? (minClampToVal, (pixel A.>* max') A.? (maxClampToVal, pixel))
    
    where minClampToVal = fromMaybe min' minClampTo
          maxClampToVal = fromMaybe max' maxClampTo

clamp' :: Maybe (A.Exp Double) -> Maybe (A.Exp Double) -> Maybe (A.Exp Double) -> Maybe (A.Exp Double) -> Generator -> Generator
clamp' min' max' minClampTo maxClampTo gen
    | Just min'' <- min', Just max'' <- max' = clamp min'' max'' minClampTo maxClampTo gen
    | Just min'' <- min'                     = Generator $ \p s -> let pixel = runGenerator gen p s in (pixel A.<* min'') A.? (fromMaybe min'' minClampTo, pixel)
    | Just max'' <- max'                     = Generator $ \p s -> let pixel = runGenerator gen p s in (pixel A.>* max'') A.? (fromMaybe max'' maxClampTo, pixel)
clamp' _ _ _ _ gen = gen

clipTest :: Generator -> Generator
clipTest gen = Generator $ \p s ->
    let pixel = runGenerator gen p s
    in (pixel A.>* 1.0) A.? (runGenerator stripes p s, (pixel A.<* 0.0) A.? (runGenerator stripes p s, pixel))

stripes :: Generator
stripes = undefined

invert :: Generator -> Generator
invert gen = Generator $ \p s -> U.invert $ runGenerator gen p s

crash :: String -> String -> a
crash f t = error $ "Function \"" ++ f ++ "\" could not be applied to Channel of type \"" ++ t ++ "\""
