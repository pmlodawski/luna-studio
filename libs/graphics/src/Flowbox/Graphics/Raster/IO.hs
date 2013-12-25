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



readImageFromBMP :: MonadIO m => FilePath -> m (Either BMP.Error (Image A.Word32))
readImageFromBMP file = liftIO(fmap mkChan <$> A.readImageFromBMP file) where
    mkChan chdata = Image.insert "rgba" (Channel.Raw chdata) mempty



writeImageToBMP :: MonadIO m => Channel.Backend A.Word32 -> FilePath -> Image A.Word32 -> m (Either Image.Error ())
writeImageToBMP backend file img = runEitherT $ do
    chan <- hoistEither $ Image.lookup "rgba" img
    let Channel.Raw mdata = Channel.compute backend chan
    liftIO $ A.writeImageToBMP file mdata
