---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Image.IO.OpenEXR (
      readFromEXR
    ) where

import           Control.Monad                  (forM)
import           Data.Char                      (toLower)

import           Flowbox.Codec.EXR              hiding (channels, name, x)
import           Flowbox.Graphics.Image.Channel (Channel (..), ChannelData (..))
import qualified Flowbox.Graphics.Image.Channel as Chan
import           Flowbox.Graphics.Image.Image   (Image)
import qualified Flowbox.Graphics.Image.Image   as Image
import           Flowbox.Graphics.Image.View    (View)
import qualified Flowbox.Graphics.Image.View    as View
import           Flowbox.Math.Matrix            as M hiding (any, (++))
import           Flowbox.Prelude                hiding (parts)



readFromEXR :: FilePath -> IO (Maybe Image)
readFromEXR path = do
    exr <- openEXRFile path
    case exr of
        Just file -> do
            partsNum <- getParts file
            parts <- forM [0..partsNum-1] $ readEXRPart file

            return $ makeImage parts
        _         -> return Nothing


readEXRPart :: EXRFile -> Int -> IO View
readEXRPart exr part = do
    channelsNames <- getChannels exr part
    channels <- forM channelsNames $ \name -> do
        floatArray <- readScanlineChannelA exr part name
        return $ ChannelFloat (convertToLunaName name) (MatrixData $ Raw floatArray)

    let newChannels = addAlphaIfAbsent channels

    partName <- maybe View.defaultName id <$> getPartName exr part
    return $ makeView partName newChannels

addAlphaIfAbsent :: [Channel] -> [Channel]
addAlphaIfAbsent [] = errorShitWentWrong $ "addAlphaIfAbsent (found an empty list) "
addAlphaIfAbsent channels@(x:_) = if alphaPresent then channels else alpha : channels
    where alphaPresent = any (\chan -> Chan.name chan == "rgba.a") channels

          ChannelFloat _ (MatrixData matrix) = x

          alpha = ChannelFloat "rgba.a" $ MatrixData $ M.fill (M.shape matrix) 1.0

convertToLunaName :: String -> String
convertToLunaName [name] = "rgba." ++ [toLower name]
convertToLunaName name   = name

makeView :: String -> [Channel] -> View
makeView name channels = foldr View.append (View.empty name) channels

makeImage :: [View] -> Maybe Image
makeImage (x:xs) = Just $ foldr Image.insert (Image.singleton x) xs
makeImage _      = Nothing

-- == HELPERS == for error reporting

errorShitWentWrong :: String -> a
errorShitWentWrong fun =
  error (thisModule ++ fun ++ ": Oh come on! No way this could happen!")

thisModule :: String
thisModule = "Flowbox.Graphics.Image.IO.OpenEXR."
