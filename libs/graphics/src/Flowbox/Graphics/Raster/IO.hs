---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module Flowbox.Graphics.Raster.IO where

import Flowbox.Prelude hiding (map)

import qualified Codec.BMP                as BMP
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import qualified Data.Array.Accelerate    as A
import qualified Data.Array.Accelerate.IO as A

import           Control.Monad.Trans.Either      (hoistEither, runEitherT)
import qualified Flowbox.Graphics.Raster.Channel as Channel
import           Flowbox.Graphics.Raster.Image   (Image)
import qualified Flowbox.Graphics.Raster.Image   as Image

import Control.Error.Util
import Control.Monad.Trans.Either


readImageFromBMP :: MonadIO m => FilePath -> m (Either BMP.Error (Image A.Word32))
readImageFromBMP file = liftIO(fmap mkChan <$> A.readImageFromBMP file) where
    mkChan chdata = Image.insert "rgba" (Channel.Raw chdata) mempty


readImageFromBMP2 :: FilePath -> IO (Either Image.Error (Image A.Word32))
readImageFromBMP2 file = do
    img  <- runEitherT (tryIO $ A.readImageFromBMP file)
    img2 <- case img of
        Left err  -> return $ Left Image.TmpError
        Right val -> case val of
            Left _ -> return $ Left Image.TmpError
            Right val2 -> return $ Right val2
    return $ fmap mkChan img2
    where mkChan chdata = Image.insert "rgba" (Channel.Raw chdata) mempty

    --liftIO(fmap mkChan <$> A.readImageFromBMP file) where
    --mkChan chdata = Image.insert "rgba" (Channel.Raw chdata) mempty



writeImageToBMP :: MonadIO m => Channel.Backend A.Word32 -> FilePath -> Image A.Word32 -> m (Either Image.Error ())
writeImageToBMP backend file img = runEitherT $ do
    chan <- hoistEither $ Image.lookup "rgba" img
    let Channel.Raw mdata = Channel.compute backend chan
    liftIO $ A.writeImageToBMP file mdata
