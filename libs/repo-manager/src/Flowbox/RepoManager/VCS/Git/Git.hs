module Flowbox.RepoManager.VCS.Git.Git where

import           Data.Bool
import           Flowbox.Prelude
import qualified Flowbox.RepoManager.VCS.Type as VCS
import qualified Flowbox.RepoManager.VCS.VCS  as VCS

import qualified System.Directory as Directory
import           System.Process   (ProcessHandle, runCommand)
import qualified Flowbox.RepoManager.Utils.Utils as Utils (concatPath)
import qualified Text.Regex.Posix as Regex

createVCS :: VCS.Type -> String -> String -> VCS.VCS
createVCS cls' repoPath remotePath' = VCS.VCS { VCS.cls = cls'
                                              , VCS.localPath = repoPath
                                              , VCS.name = getName remotePath'
                                              , VCS.remotePath = remotePath'
                                              , VCS.clone = clone
                                              , VCS.update = update
                                              , VCS.remove = remove
                                              }
    where getName remotePath = head.tail.head $ (remotePath Regex.=~ "([A-Za-z_]*).git$" :: [[String]])

remove :: VCS.VCS -> IO ProcessHandle
remove repo = do runCommand $ concat ["rm -fR ", Utils.concatPath [VCS.localPath repo , VCS.name repo]]

clone :: VCS.VCS -> IO VCS.VCS
clone repo = do Directory.createDirectoryIfMissing True (VCS.localPath repo)
                current <- Directory.getCurrentDirectory
                Directory.setCurrentDirectory (VCS.localPath repo)
                runCommand $ concat ["git clone --quiet ", (VCS.remotePath repo)]
                Directory.setCurrentDirectory current
                return repo

update :: VCS.VCS -> IO VCS.VCS
update repo = do current <- Directory.getCurrentDirectory
                 Directory.setCurrentDirectory (Utils.concatPath [VCS.localPath repo, VCS.name repo])
                 runCommand $ concat ["git pull --quiet ", VCS.remotePath repo] -- ADD --quiet?
                 Directory.setCurrentDirectory current
                 return repo


