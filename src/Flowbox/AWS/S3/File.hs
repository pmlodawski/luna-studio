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

import           Flowbox.AWS.S3.S3       (S3)
import qualified Flowbox.AWS.S3.S3       as S3
import           Flowbox.Prelude
import qualified Flowbox.System.FilePath as FilePath



fetch :: FilePath -> S3 ()
fetch filePath = S3.withBucket $ \bucket -> do
    rsp <- S3.query $ S3.getObject bucket $ Text.pack filePath
    lift $ HTTP.responseBody (S3.gorResponse rsp) $$+- Binary.sinkFile filePath
    return ()


upload :: FilePath -> S3 ()
upload filePath = S3.withBucket $ \bucket -> do
    let normFilePath = FilePath.normalise' filePath
    file <- liftIO $ HTTP.RequestBodyBS <$> (ByteString.readFile normFilePath)
    _ <- S3.query $ S3.putObject bucket (Text.pack normFilePath) file
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


removeMany :: [FilePath] -> S3 ()
removeMany filePaths = case filePaths of
    [] -> return ()
    _  -> S3.withBucket $ \bucket -> do
            _ <- S3.query $ S3.deleteObjects bucket (map Text.pack filePaths)
            return ()


rename :: FilePath -> FilePath -> S3 ()
rename srcFilePath dstFilePath = do
    let normDstFilePath = FilePath.normalise' dstFilePath
    copy srcFilePath normDstFilePath
    remove srcFilePath


copy :: FilePath -> FilePath -> S3 ()
copy srcFilePath dstFilePath = S3.withBucket $ \bucket -> do
    let normDstFilePath = FilePath.normalise' dstFilePath
        src = S3.ObjectId bucket (Text.pack srcFilePath) Nothing
    _ <- S3.query $ S3.copyObject bucket (Text.pack normDstFilePath) src S3.CopyMetadata
    return ()


create :: FilePath -> S3 ()
create filePath = S3.withBucket $ \bucket -> do
    let normFilePath = FilePath.normalise' filePath
        file         = HTTP.RequestBodyBS $ ByteString.empty
    _ <- S3.query $ S3.putObject bucket (Text.pack normFilePath) file
    return ()
