---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module Flowbox.Graphics.Image.IO where

import Flowbox.Prelude hiding (map)

import qualified Codec.BMP                as BMP
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import qualified Data.Array.Accelerate    as A
import qualified Data.Array.Accelerate.IO as A

import           Flowbox.Graphics.ImageRGBA     (ImageRGBA)
import           Flowbox.Graphics.Image         (Image)
import qualified Flowbox.Graphics.Image         as Image
import           Flowbox.Graphics.Image.Channel (Channel2, Channel3)
import qualified Flowbox.Graphics.Image.Channel as Channel

import Control.Monad.Trans.Either


--readFromBMP :: (MonadIO m, Image img (Channel2 A.Word32)) => FilePath -> m (Either BMP.Error (img (Channel2 A.Word32)))
readFromBMP :: MonadIO m => FilePath -> m (Either BMP.Error (ImageRGBA (Channel2 A.Word32)))
readFromBMP file = liftIO(fmap mkChan <$> A.readImageFromBMP file) where
    mkChan chdata = Image.insert "rgba" (Channel.Raw chdata) mempty

--readSequenceFromBMP ::(MonadIO m, Image img (Channel3 A.Word32)) => [FilePath] -> m (Either BMP.Error (img (Channel3 A.Word32)))
readSequenceFromBMP ::MonadIO m => [FilePath] -> m (Either BMP.Error (ImageRGBA (Channel3 A.Word32)))
readSequenceFromBMP paths = liftIO (fmap mkChanSequence <$> prepareSequence paths) where
    mkChanSequence chsdata = Image.insert "rgba" (Channel.Acc chsdata) mempty
    prepareSequence [] = return $ Right $ A.use $ A.fromList (A.Z A.:. 0 A.:. 0 A.:. 0) []
    prepareSequence (file:[]) = fmap lift2Dto3D <$> A.readImageFromBMP file
    prepareSequence (file:files) = (liftA2.liftA2) (A.++) (fmap lift2Dto3D <$> A.readImageFromBMP file) (prepareSequence files)
    lift2Dto3D rawArray = let
            array = A.use rawArray
            (A.Z A.:. a A.:. b) = A.unlift $ A.shape array
        in A.reshape (Channel.index3 a b 1) array

--readImageFromBMP2 :: FilePath -> IO (Image.Result (Image A.Word32))
--readImageFromBMP2 file = do
--    img  <- runEitherT (tryIO $ A.readImageFromBMP file)
--    img2 <- case img of
--        Left err  -> return $ Left Image.TmpError
--        Right val -> case val of
--            Left _ -> return $ Left Image.TmpError
--            Right val2 -> return $ Right val2
--    return $ fmap mkChan img2
--    where mkChan chdata = Image.insert "rgba" (Channel.Raw chdata) mempty

--    --liftIO(fmap mkChan <$> A.readImageFromBMP file) where
--    --mkChan chdata = Image.insert "rgba" (Channel.Raw chdata) mempty



writeToBMP :: (MonadIO m, Image img (Channel2 A.Word32)) => Channel.Backend A.DIM2 A.Word32 -> FilePath -> img (Channel2 A.Word32) -> m (Image.Result ())
writeToBMP backend file img = runEitherT $ do
    chan <- hoistEither $ Image.get "rgba" img
    let Channel.Raw mdata = Channel.compute backend chan
    liftIO $ A.writeImageToBMP file mdata
