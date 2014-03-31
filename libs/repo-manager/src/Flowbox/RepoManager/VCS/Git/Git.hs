module Flowbox.RepoManager.VCS.Git.Git where

import  System.Directory as F 
import System.Process as P
import Data.Bool 
import Data.List (concat)
import Flowbox.Prelude


--import Git.Libgit2.Repository as LG
--import  Data.Maybe


--initRepository repoPath = do
--                             repo   <- openOrCreateRepository repoPath False
--                             ref    <- resolveRef repo "HEAD"

--                             -- commit <- maybe (return Nothing) (lookupCommit repo) ref
 
removeRepository repoPath = do exists <- F.doesDirectoryExist repoPath
                               let result = case exists of
                                                True -> Just $ P.runCommand $ concat ["rm -fR ", repoPath]
                                                False -> Nothing
                               return exists

-- catch exceptions
cloneRepository repoPath remote = do   
                                       F.createDirectoryIfMissing True repoPath
                                       current <- getCurrentDirectory
                                       F.setCurrentDirectory repoPath
                                       P.runCommand $ concat ["git clone ", remote]
                                       F.setCurrentDirectory current

pullRepository repoPath remote = do P.runCommand $ concat ["git pull ", remote, " ", repoPath]
                                     
--removeRepository repoPath = F.doesDirectoryExist repoPath