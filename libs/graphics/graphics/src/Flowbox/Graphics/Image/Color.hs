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
import           Flowbox.Math.Matrix                                as M
import           Flowbox.Prelude                                    as P



mapInt :: (Exp Int -> Exp Int) -> Channel -> Channel
mapInt f chan = case chan of
    Channel.ChannelInt name (Channel.FlatData m) -> Channel.ChannelInt name $ Channel.FlatData $ M.map f m
    _ -> error "Function from Int to Int could not be applied to this type of Channel!"

mapFloat :: (Exp Double -> Exp Double) -> Channel -> Channel
mapFloat f chan = case chan of
    Channel.ChannelFloat name (Channel.FlatData m) -> Channel.ChannelFloat name $ Channel.FlatData $ M.map f m
    _ -> error "Function from Double to Double could not be applied to this type of Channel!"

mapBool :: (Exp Bool -> Exp Bool) -> Channel -> Channel
mapBool f chan = case chan of
    Channel.ChannelBit name (Channel.FlatData m) -> Channel.ChannelBit name $ Channel.FlatData $ M.map f m
    _ -> error "Function from Bool to Bool could not be applied to this type of Channel!"

offset :: Exp Double -> Channel -> Channel
offset v chan | Channel.ChannelInt{}   <- chan = mapInt (+ (A.truncate v)) chan
              | Channel.ChannelFloat{} <- chan = mapFloat (+ v) chan
              | otherwise                      = crash "offset" "Bool"

offset' :: (Elt a, IsNum a) => Exp a -> Generator a -> Generator a
offset' v gen = Generator $ \p s -> runGenerator gen p s + v

multiply :: Exp Double -> Channel -> Channel
multiply v chan | Channel.ChannelInt{}   <- chan = mapInt (* (A.truncate v)) chan
                | Channel.ChannelFloat{} <- chan = mapFloat (* v) chan
                | otherwise                      = crash "multiply" "Bool"

multiply' :: (Elt a, IsNum a) => Exp a -> Generator a -> Generator a
multiply' v gen = Generator $ \p s -> runGenerator gen p s * v

not :: Channel -> Channel
not chan | Channel.ChannelBit{} <- chan = mapBool (A.not) chan
         | otherwise                    = crash "not" "Numeric"

contrast :: Exp Double -> Channel -> Channel
contrast v chan | Channel.ChannelInt{}   <- chan = mapInt (\x -> A.truncate $ (A.fromIntegral x - 0.5) * v + 0.5) chan
                | Channel.ChannelFloat{} <- chan = mapFloat (\x -> (x - 0.5) * v + 0.5) chan
                | otherwise                      = crash "contrast" "Bool"

contrast' :: (Elt a, IsFloating a) => Exp a -> Generator a -> Generator a
contrast' v gen = Generator $ \p s -> (runGenerator gen p s - 0.5) * v + 0.5

gamma :: Exp Double -> Channel -> Channel
gamma v chan | Channel.ChannelInt{}   <- chan = mapInt (\x -> A.truncate $ A.fromIntegral x ** (1/v)) chan
             | Channel.ChannelFloat{} <- chan = mapFloat (** (1/v)) chan
             | otherwise                      = crash "gamma" "Bool"

gamma' :: (Elt a, IsFloating a) => Exp a -> Generator a -> Generator a
gamma' v gen = Generator $ \p s -> runGenerator gen p s ** (1/v)

clamp :: (Elt a, IsScalar a) => Exp a -> Exp a -> Maybe (Exp a) -> Maybe (Exp a) -> Generator a -> Generator a
clamp min' max' minClampTo maxClampTo gen = Generator $ \p s -> 
    let pixel = runGenerator gen p s
    in (pixel A.<* min') A.? (minClampToVal, (pixel A.>* max') A.? (maxClampToVal, pixel))
    
    where minClampToVal = fromMaybe min' minClampTo
          maxClampToVal = fromMaybe max' maxClampTo

clamp' :: (Elt a, IsScalar a) => Maybe (Exp a) -> Maybe (Exp a) -> Maybe (Exp a) -> Maybe (Exp a) -> Generator a -> Generator a
clamp' min' max' minClampTo maxClampTo gen
    | Just min'' <- min', Just max'' <- max' = clamp min'' max'' minClampTo maxClampTo gen
    | Just min'' <- min'                     = Generator $ \p s -> let pixel = runGenerator gen p s in (pixel A.<* min'') A.? (fromMaybe min'' minClampTo, pixel)
    | Just max'' <- max'                     = Generator $ \p s -> let pixel = runGenerator gen p s in (pixel A.>* max'') A.? (fromMaybe max'' maxClampTo, pixel)
clamp' _ _ _ _ gen = gen

clipTest :: (Elt a, IsScalar a, IsFloating a) => Generator a -> Generator a
clipTest gen = Generator $ \p s ->
    let pixel = runGenerator gen p s
    in (pixel A.>* 1.0) A.? (runGenerator stripes p s, (pixel A.<* 0.0) A.? (runGenerator stripes p s, pixel))

stripes :: Generator a
stripes = undefined

invert :: (Elt a, IsNum a) => Generator a -> Generator a
invert gen = Generator $ \p s -> U.invert $ runGenerator gen p s

crash :: String -> String -> a
crash f t = error $ "Function \"" P.++ f P.++ "\" could not be applied to Channel of type \"" P.++ t P.++ "\""
