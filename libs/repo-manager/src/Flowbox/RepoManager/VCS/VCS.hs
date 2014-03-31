module Flowbox.RepoManager.VCS.VCS where

import Flowbox.RepoManager.VCS.Git.Git as Git 
import Data.List as L
import System.FilePath (pathSeparator)

--updateRepository localPath remote
--    - no repo? -> clone
--    - exists? -> pull

--Add. windows file manipulations?
createRepository localPath remote = Git.cloneRepository localPath remote

updateRepository localPath remote = Git.pullRepository localPath remote 

removeRepository repoPath = Git.removeRepository repoPath
                                        
path directories = L.intercalate [pathSeparator] directories



