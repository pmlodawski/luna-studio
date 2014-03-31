module Flowbox.RepoManager.VCS.VCS where

import Flowbox.RepoManager.VCS.Git.Git as Git 
import Data.List as L
import System.FilePath (pathSeparator)
import Flowbox.RepoManager.VCS.Type as Type


create Type.Git repoPath name remote = do 
                                                let vcs = Type.VCS { type_ = Type.Git
                                                                   , local = repoPath
                                                                   , name = name
                                                                   , remote = remote
                                                                   }
                                                Git.clone vcs

update repo = Git.pull repo

remove repo = Git.remove repo
                                        
path directories = L.intercalate [pathSeparator] directories



