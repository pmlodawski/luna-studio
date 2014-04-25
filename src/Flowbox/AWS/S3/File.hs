---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AWS.S3.File where

import           Control.Monad.Reader
import qualified Data.ByteString      as ByteString
import           Data.Conduit         (($$+-))
import qualified Data.Conduit.Binary  as Binary
import qualified Data.Text            as Text
import qualified Network.HTTP.Conduit as HTTP

import           Flowbox.AWS.S3.S3 (S3)
import qualified Flowbox.AWS.S3.S3 as S3
import           Flowbox.Prelude



fetch :: FilePath -> S3 ()
fetch filePath = S3.withBucket $ \bucket -> do
    rsp <- S3.query $ S3.getObject bucket $ Text.pack filePath
    lift $ HTTP.responseBody (S3.gorResponse rsp) $$+- Binary.sinkFile filePath
    return ()


upload :: FilePath -> S3 ()
upload filePath = S3.withBucket $ \bucket -> do
    file <- liftIO $ HTTP.RequestBodyBS <$> ByteString.readFile filePath
    _ <- S3.query $ S3.putObject bucket (Text.pack filePath) file
    return ()


exists :: FilePath -> S3 Bool
exists filePath = S3.withBucket $ \bucket -> do
    rsp <- S3.query $ (S3.getBucket bucket) { S3.gbPrefix = Just $ Text.pack filePath }
    let contents = S3.gbrContents rsp
        matching = filter ((==) (Text.pack filePath)) $ map S3.objectKey contents
    return $ length matching == 1


remove :: FilePath -> S3 ()
remove filePath = S3.withBucket $ \bucket -> do
    _ <- S3.query $ S3.DeleteObject (Text.pack filePath) bucket
    return ()


rename :: FilePath -> FilePath -> S3 ()
rename srcFilePath dstFilePath = do
    copy srcFilePath dstFilePath
    remove srcFilePath


copy :: FilePath -> FilePath -> S3 ()
copy srcFilePath dstFilePath = S3.withBucket $ \bucket -> do
    let src = S3.ObjectId bucket (Text.pack srcFilePath) Nothing
    _ <- S3.query $ S3.copyObject bucket (Text.pack dstFilePath) src S3.CopyMetadata
    return ()


touch :: FilePath -> S3 ()
touch filePath = S3.withBucket $ \bucket -> do
    let file = HTTP.RequestBodyBS $ ByteString.empty
    _ <- S3.query $ S3.putObject bucket (Text.pack filePath) file
    return ()
