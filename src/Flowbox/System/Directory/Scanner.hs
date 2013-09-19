---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.System.Directory.Scanner where

import           Control.Applicative      
import qualified Data.List              as List
import qualified System.Directory       as Directory

import           Flowbox.Prelude          
import qualified Flowbox.System.UniPath as UniPath
import           Flowbox.System.UniPath   (UniPath)


getDirectoryRecursive :: UniPath -> IO [UniPath]
getDirectoryRecursive upath = do
    path <- UniPath.toUnixString <$> UniPath.expand upath
    isDir <- Directory.doesDirectoryExist path
    if isDir 
        then do paths <- Directory.getDirectoryContents path
                children <- mapM getDirectoryRecursive $ map UniPath.fromUnixString paths
                let upaths = map UniPath.fromUnixString paths
                return $ upaths ++ (List.concat children)
        else return [upath]



scan :: UniPath -> (UniPath -> Bool) -> IO [UniPath]
scan rootPath predicate = filter predicate <$> getDirectoryRecursive rootPath


scanByExts :: UniPath -> String -> IO [UniPath]
scanByExts rootPath extension = scan rootPath hasExtension where
    hasExtension path = (UniPath.extension path) == extension