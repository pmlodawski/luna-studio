-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.System.Directory.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    getDirectoryRecursive,

    module System.Directory,
) where

import           Control.Applicative      
import qualified Data.List              as List
import qualified System.Directory       as Directory
import           System.Directory       hiding (createDirectoryIfMissing, doesDirectoryExist)

import           Flowbox.Prelude          
import qualified Flowbox.System.UniPath as UniPath
import           Flowbox.System.UniPath   (UniPath)



getDirectoryRecursive :: UniPath -> IO [UniPath]
getDirectoryRecursive upath = do
    path  <- UniPath.expand upath
    isDir <- doesDirectoryExist path
    if isDir
        then do paths <- Directory.getDirectoryContents $ UniPath.toUnixString path
                let filtered = filter (/= ".") $ filter (/= "..") paths
                    upaths = map (\a -> UniPath.append a path) filtered
                children <- mapM getDirectoryRecursive upaths
                return $ List.concat children
        else return [path]


createDirectoryIfMissing :: Bool -> UniPath -> IO ()
createDirectoryIfMissing create_parents upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath
    Directory.createDirectoryIfMissing create_parents path


doesDirectoryExist :: UniPath -> IO Bool
doesDirectoryExist upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath
    Directory.doesDirectoryExist path
