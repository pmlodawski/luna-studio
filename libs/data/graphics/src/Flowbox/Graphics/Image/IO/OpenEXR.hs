---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Image.IO.OpenEXR where

import qualified Data.Array.Accelerate as A
import           Control.Monad         (forM)

import           Flowbox.Codec.EXR
import           Flowbox.Graphics.Image.Channel
import           Flowbox.Graphics.Image.Image   (Image)
import qualified Flowbox.Graphics.Image.Image   as Image
import           Flowbox.Graphics.Image.View    (View)
import qualified Flowbox.Graphics.Image.View    as View
import           Flowbox.Math.Matrix            as M
import           Flowbox.Prelude

import GHC.Float



readFromEXR :: FilePath -> IO (Maybe Image)
readFromEXR path = do
	exr <- openEXRFile path
	case exr of
		Just file -> do
			partsNum <- getParts file
			parts <- forM [0..partsNum-1] $ readEXRPart file

			return $ Just $ makeImage parts
		_         -> return Nothing


readEXRPart :: EXRFile -> Int -> IO View
readEXRPart exr part = do
	channelsNames <- getChannels exr part
	channels <- forM channelsNames $ \name -> do
		floatArray <- readScanlineChannelA exr part name
		return $ ChannelFloat name (FlatData $ M.map realToFrac $ Raw floatArray)

	partName <- maybe "" id <$> getPartName exr part
	return $ makeView partName channels

makeView :: String -> [Channel] -> View
makeView name channels = foldr View.append (View.empty name) channels

makeImage :: [View] -> Image
makeImage (x:xs) = foldr Image.insert (Image.singleton x) xs
