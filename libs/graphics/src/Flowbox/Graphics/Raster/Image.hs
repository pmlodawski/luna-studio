{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Graphics.Raster.Image where

import Flowbox.Prelude hiding(map, lookup)

import qualified Data.Array.Accelerate         as A
import           Data.Array.Accelerate         (Acc, Exp, (:.)(..))
import qualified Data.Map                      as Map
import           Data.Map                      (Map)
import           Data.Monoid                   (mempty, Monoid)
import           Control.Lens
import           Control.Error

import qualified Flowbox.Graphics.Raster.Channel as Channel
import           Flowbox.Graphics.Raster.Channel (Channel)

data Image a = Image { _channels :: Map String (Channel a)
                     }
             deriving (Show)

makeLenses ''Image


data Error = LookupError
           deriving (Show)

map :: (A.Elt a, A.Elt b) => (Exp a -> Exp b) -> Image a -> Image b
map f img = Image $ Map.map (Channel.map f) $ view channels img

compute img = Image $ Map.map Channel.compute $ view channels img

lookup :: String -> Image a -> Maybe (Channel a)
lookup name img = Map.lookup name (view channels img)


lookup' :: String -> Image a -> Either Error (Channel a)
lookup' name img = justErr LookupError $ Map.lookup name (view channels img)

cpChannel :: String -> String -> Image a -> Image a
cpChannel src dst img = case chan of
	Nothing -> img
	Just ch -> img & channels %~ (Map.insert dst ch)
	where chan = lookup src img
	

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
	mempty = Image mempty