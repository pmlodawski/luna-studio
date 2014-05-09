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
import qualified System.Directory     as Directory
import           System.FilePath      ((</>))
import qualified System.FilePath      as FilePath

import           Flowbox.AWS.S3.S3       (S3)
import qualified Flowbox.AWS.S3.S3       as S3
import           Flowbox.Prelude
import qualified Flowbox.System.FilePath as FilePath



fetch :: FilePath -> FilePath -> S3 ()
fetch basePath filePath = S3.withBucket $ \bucket -> do
    rsp <- S3.query $ S3.getObject bucket $ Text.pack filePath
    liftIO $ Directory.createDirectoryIfMissing True $ FilePath.takeDirectory (basePath </> filePath)
    lift $ HTTP.responseBody (S3.gorResponse rsp) $$+- Binary.sinkFile (basePath </> filePath)


upload :: FilePath -> FilePath -> S3 ()
upload basePath filePath = S3.withBucket $ \bucket -> do
    let normFilePath = FilePath.normalise' filePath
    file <- liftIO $ HTTP.RequestBodyBS <$> (ByteString.readFile $ basePath </> normFilePath)
    void $ S3.query $ S3.putObject bucket (Text.pack normFilePath) file


exists :: FilePath -> S3 Bool
exists filePath = S3.withBucket $ \bucket -> do
    rsp <- S3.query $ (S3.getBucket bucket) { S3.gbPrefix = Just $ Text.pack filePath }
    let contents = S3.gbrContents rsp
    return $ Text.pack filePath `elem` map S3.objectKey contents


remove :: FilePath -> S3 ()
remove filePath = S3.withBucket $ \bucket ->
    void $ S3.query $ S3.DeleteObject (Text.pack filePath) bucket


removeMany :: [FilePath] -> S3 ()
removeMany filePaths = case filePaths of
    [] -> return ()
    _  -> S3.withBucket $ \bucket -> void $ S3.query $ S3.deleteObjects bucket (map Text.pack filePaths)


rename :: FilePath -> FilePath -> S3 ()
rename srcFilePath dstFilePath = do
    let normDstFilePath = FilePath.normalise' dstFilePath
    copy   srcFilePath normDstFilePath
    remove srcFilePath


copy :: FilePath -> FilePath -> S3 ()
copy srcFilePath dstFilePath = S3.withBucket $ \bucket -> do
    let normDstFilePath = FilePath.normalise' dstFilePath
        src             = S3.ObjectId bucket (Text.pack srcFilePath) Nothing
    void $ S3.query $ S3.copyObject bucket (Text.pack normDstFilePath) src S3.CopyMetadata


create :: FilePath -> S3 ()
create filePath = S3.withBucket $ \bucket -> do
    let normFilePath = FilePath.normalise' filePath
        file         = HTTP.RequestBodyBS $ ByteString.empty
    void $ S3.query $ S3.putObject bucket (Text.pack normFilePath) file
