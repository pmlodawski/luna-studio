---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Graphics.Raster.Image (
    module Flowbox.Graphics.Raster.Image,
    Error(..)
) where

import Flowbox.Prelude hiding (lookup, map)

import           Control.Error
import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A
import           Data.Map              (Map)
import qualified Data.Map              as Map

import           Flowbox.Graphics.Raster.Channel (Channel)
import qualified Flowbox.Graphics.Raster.Channel as Channel
import           Flowbox.Graphics.Raster.Error   (Error (ChannelLookupError))

data Image a = Image { _channels :: Map String (Channel a)
                     }
             deriving (Show, Eq, Ord)

makeLenses ''Image


compute :: Channel.Backend a -> Image a -> Image a
compute backend img = Image $ Map.map (Channel.compute backend) $ view channels img


map :: (A.Elt a, A.Elt b) => (Exp a -> Exp b) -> Image a -> Image b
map f img = Image $ Map.map (Channel.map f) $ view channels img


lookup :: String -> Image a -> Either Error (Channel a)
lookup name img = justErr (ChannelLookupError name) $ Map.lookup name (view channels img)


cpChannel :: String -> String -> Image a -> Either Error (Image a)
cpChannel src dst img = do
    chan <- lookup src img
    return $ img & channels %~ (Map.insert dst chan)


insert :: String -> Channel a -> Image a -> Image a
insert name chan img = img & channels %~ (Map.insert name chan)

reprFloat :: Image A.Word8 -> Image A.Float
reprFloat img = map (\c -> A.fromIntegral c / 255) img

reprWord8 :: Image A.Float -> Image A.Word8
reprWord8 img = map (\c -> A.truncate $ c * 255) img

------------------------------------------------------------------------
-- INSTANCES
------------------------------------------------------------------------

instance Monoid (Image a) where
    mempty        = Image mempty
    a `mappend` b = Image $ (view channels a) `mappend` (view channels b)
