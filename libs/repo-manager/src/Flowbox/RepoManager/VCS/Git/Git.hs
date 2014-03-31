module Flowbox.RepoManager.VCS.Git.Git where

import Data.Bool
--import           Data.List                    (concat)
import           Flowbox.Prelude
import qualified Flowbox.RepoManager.VCS.Type as VCS
import qualified Flowbox.RepoManager.VCS.VCS  as VCS

import qualified System.Directory as Directory
import           System.FilePath  (pathSeparator)
import           System.Process   (ProcessHandle, runCommand)

createVCS :: VCS.Type -> String -> String -> String -> VCS.VCS
createVCS cls' repoPath name' remotePath' = VCS.VCS { VCS.cls = cls'
                                                   , VCS.localPath = repoPath
                                                   , VCS.name = name'
                                                   , VCS.remotePath = remotePath'
                                                   , VCS.clone = clone
                                                   , VCS.update = update
                                                   , VCS.remove = remove
                                                   }

remove :: VCS.VCS -> IO ProcessHandle
remove repo = do runCommand $ concat ["rm -fR ", VCS.localPath repo , [pathSeparator], VCS.name repo]

clone :: VCS.VCS -> IO VCS.VCS
clone repo = do
                Directory.createDirectoryIfMissing True (VCS.localPath repo)
                current <- Directory.getCurrentDirectory
                Directory.setCurrentDirectory (VCS.localPath repo)
                runCommand $ concat ["git clone ", (VCS.remotePath repo)] -- ADD --quiet?
                Directory.setCurrentDirectory current
                return repo

update :: VCS.VCS -> IO VCS.VCS
update repo = do
               current <- Directory.getCurrentDirectory
               Directory.setCurrentDirectory (concat [VCS.localPath repo, [pathSeparator], VCS.name repo])
               runCommand $ concat ["git pull ", VCS.remotePath repo] -- ADD --quiet?
               Directory.setCurrentDirectory current
               return repo


