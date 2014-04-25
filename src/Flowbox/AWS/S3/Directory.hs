---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AWS.S3.Directory where

import qualified Data.List as List
import qualified Data.Text as Text

import           Flowbox.AWS.S3.S3 (S3)
import qualified Flowbox.AWS.S3.S3 as S3
import           Flowbox.Prelude



--TODO [PM] : Find more effective way
getContents :: FilePath -> S3 [FilePath]
getContents filePath =
    List.nub . map (takeWhile ((/=) '/')) <$> getContentsRecurisively filePath


getContentsRecurisively :: FilePath -> S3 [FilePath]
getContentsRecurisively filePath = S3.withBucket $ \bucket -> do
    rsp <- S3.query $ (S3.getBucket bucket) { S3.gbPrefix = Just $ Text.pack filePath }
    return $ map (Text.unpack . S3.objectKey) $ S3.gbrContents rsp


--create :: FilePath -> IO ()


--createIfMissing :: FilePath -> IO ()


exists :: FilePath -> S3 Bool
exists filePath = S3.withBucket $ \bucket -> do
    rsp <- S3.query $ (S3.getBucket bucket) { S3.gbPrefix = Just $ Text.pack filePath }
    let contents = S3.gbrContents rsp
        matching = filter ((==) (Text.pack filePath)) $ map S3.objectKey contents
    return $ length matching > 0

--rename :: FilePath -> FilePath -> IO ()


--remove :: FilePath -> IO ()


--removeRecursive :: FilePath -> IO ()
