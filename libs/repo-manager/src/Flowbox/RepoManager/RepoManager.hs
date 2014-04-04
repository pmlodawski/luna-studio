---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.RepoManager (
      -- initRepository
     searchRepository
) where

import Flowbox.Prelude
import qualified Text.Regex.Posix as Regex
import qualified Data.Map as Map
import qualified Flowbox.RepoManager.Data.Repository as Repository
import qualified Flowbox.RepoManager.Data.Item.Item as Item
import qualified Flowbox.RepoManager.Data.Item.Name as Item
import qualified System.Directory as Directory
import qualified Flowbox.RepoManager.Utils.Utils as Utils (concatPath)
import qualified Flowbox.RepoManager.VCS.Git.Git as Git
import qualified Flowbox.RepoManager.VCS.Type as VCS


--initRepository :: FilePath -> IO Repository.Repository
--initRepository filePath = let Git.createVCS VCS.Git repoPath remotePath'
                             
--                             if Directory.doesDirectoryExist $ Utils.concatPath [filePath, ".git"]
--                             then 
    -- check if repo exists
        -- si, read repo
        -- no, clone repo
 --forall source.Regex.RegexMaker
 --                           Regex.Regex Regex.CompOption Regex.ExecOption source =>
 --                         Repository.Repository
 --                         -> source -> Map.Map Item.Name [Data.Version.Version]

searchRepository :: Repository.Repository -> String -> [Item.Name]
searchRepository repo expression = Map.keys $ Map.filterWithKey match items
    where match key _value = key Regex.=~ expression :: Bool
          items = Repository.items repo 