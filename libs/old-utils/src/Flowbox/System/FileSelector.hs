---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.System.FileSelector where

import qualified Control.Monad.Loops as Loops
import qualified Data.List           as List
import           System.FilePath     ((</>))
import qualified System.FilePath     as FilePath

import           Flowbox.Prelude
import qualified Flowbox.System.Directory as Directory
import           Flowbox.System.UniPath   (UniPath)
import qualified Flowbox.System.UniPath   as UniPath



selectWith :: [UniPath] -> (FilePath -> IO Bool) -> IO (Maybe FilePath)
selectWith []            _        = return Nothing
selectWith (l:locations) selector = do
    exists <- Directory.doesDirectoryExist l
    if exists
        then do
            l' <- toString <$> UniPath.expand l
            contents <- map (l' </>) <$> Directory.getDirectoryContents l
            Loops.firstM selector contents >>= \case
                Just file -> return $ Just file
                Nothing  -> next
        else next
    where
        next = selectWith locations selector


selectWithExt :: [UniPath] -> FilePath -> IO (Maybe FilePath)
selectWithExt locations ext = selectWith locations selector where
    selector filePath = if List.isSuffixOf ext filePath
        then Directory.doesFileExist $ fromString filePath
        else return False
