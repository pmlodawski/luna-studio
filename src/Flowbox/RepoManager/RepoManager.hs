---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.RepoManager where

import Flowbox.Prelude
import qualified Text.Regex.Posix as Regex
import qualified Data.Map as Map
import qualified Flowbox.RepoManager.Data.Repository as Repository
import qualified Flowbox.RepoManager.Data.Item.Name as Item
import qualified System.Directory as Directory
import qualified Flowbox.RepoManager.Utils.Utils as Utils (concatPath)
import qualified Flowbox.RepoManager.VCS.Git.Git as Git
import qualified Flowbox.RepoManager.VCS.Type as VCS

initRepository :: FilePath -> String -> IO Repository.Repository
initRepository filePath remotePath = do let vcs = Git.createVCS VCS.Git filePath remotePath
                                        exists <- Directory.doesDirectoryExist $ Utils.concatPath [filePath, ".git"]
                                        if exists
                                            then Repository.buildRepository vcs
                                            else Git.clone vcs >>= Repository.buildRepository

searchRepository :: Repository.Repository -> String -> [Item.Name]
searchRepository repo expression = Map.keys $ Map.filterWithKey match items
    where match key _value = key Regex.=~ expression :: Bool
          items = Repository.items repo 