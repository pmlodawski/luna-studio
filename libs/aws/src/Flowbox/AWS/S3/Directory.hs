---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AWS.S3.Directory where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.String.Utils      as String
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified System.Directory       as Directory
import           System.FilePath        ((</>))
import qualified System.FilePath        as FilePath

import qualified Flowbox.AWS.S3.File                as File
import           Flowbox.AWS.S3.S3                  (S3)
import qualified Flowbox.AWS.S3.S3                  as S3
import qualified Flowbox.AWS.S3.Utils               as Utils
import           Flowbox.Prelude
import qualified Flowbox.System.Directory.Directory as FDirectory



fetch :: FilePath -> FilePath -> S3 ()
fetch basePath filePath = do
    contents <- getContentsRecurisively filePath
    mapM_ fetchItem contents
    where
        fetchItem path = if Utils.isDirectory path
            then liftIO $ Directory.createDirectoryIfMissing True (basePath </> path)
            else File.fetch basePath path


upload :: FilePath -> FilePath -> S3 ()
upload basePath filePath = do
    contents <- liftIO $ FDirectory.listDirectory $ basePath </> filePath
    mapM_ (uploadItem . FilePath.combine filePath) contents
    where
        uploadItem path = do
            isDir <- liftIO $ Directory.doesDirectoryExist $ basePath </> path
            if isDir
                then do create path
                        upload basePath path
                else File.upload basePath path


directoryPrefix :: FilePath -> Maybe Text
directoryPrefix filePath = case filePath of
    "." -> Nothing
    p   -> Just $ Text.pack $ if Utils.isDirectory p
                                then p
                                else p ++ "/"


getContents :: FilePath -> S3 [FilePath]
getContents filePath = S3.withBucket $ \bucket -> do
    let prefix = directoryPrefix filePath
    rsp <- S3.query $ (S3.getBucket bucket) { S3.gbPrefix = prefix, S3.gbDelimiter = Just $ Text.pack Utils.dirMarker }
    return $ map Text.unpack $ S3.gbrCommonPrefixes rsp ++ map S3.objectKey (S3.gbrContents rsp)


getContentsRecurisively :: FilePath -> S3 [FilePath]
getContentsRecurisively filePath = S3.withBucket $ \bucket -> do
    let prefix = directoryPrefix filePath
    rsp <- S3.query $ (S3.getBucket bucket) { S3.gbPrefix = prefix }
    return $ map (Text.unpack . S3.objectKey) $ S3.gbrContents rsp


create :: FilePath -> S3 ()
create filePath = File.create $ Utils.normaliseDir filePath


exists :: FilePath -> S3 Bool
exists = File.exists


copy :: FilePath -> FilePath -> S3 ()
copy srcFilePath dstFilePath = do
    let normDstFilePath = Utils.normaliseDir dstFilePath
    contents <- getContentsRecurisively srcFilePath
    let copies = zip contents $ map (replacePathBase srcFilePath normDstFilePath) contents
    mapM_ (uncurry File.copy) copies


rename :: FilePath -> FilePath -> S3 ()
rename srcFilePath dstFilePath = do
    let normDstFilePath = Utils.normaliseDir dstFilePath
    contents <- getContentsRecurisively srcFilePath
    let renames = zip contents $ map (replacePathBase srcFilePath normDstFilePath) contents
    mapM_ (uncurry File.rename) renames


remove :: FilePath -> S3 ()
remove filePath = do
    contents <- getContentsRecurisively filePath
    File.removeMany contents

--removeRecursive :: FilePath -> S3 ()

replacePathBase :: FilePath -> FilePath -> FilePath -> FilePath
replacePathBase old new l = case (old, new) of
    (".", _) -> new </> l
    (_, ".") -> String.replace old ""  l
    _        -> String.replace old new l
