module Flowbox.RepoManager.VCS.Git.Git where

import  System.Directory as Directory
import System.Process (runCommand)
import Data.Bool 
import Data.List (concat)
import Flowbox.Prelude
import System.FilePath (pathSeparator)
import Flowbox.RepoManager.VCS.Type as Type


--import Git.Libgit2.Repository as LG
--import  Data.Maybe


--initRepository local = do
--                             repo   <- openOrCreateRepository local False
--                             ref    <- resolveRef repo "HEAD"

--                             -- commit <- maybe (return Nothing) (lookupCommit repo) ref
 
remove repo = do exists <- Directory.doesDirectoryExist $ concat [(Type.local repo), [pathSeparator], (Type.name repo)]
                 let result = case exists of
                            True -> Just $ runCommand $ concat ["rm -fR ", Type.local repo , [pathSeparator], Type.name repo]
                            False -> Nothing
                 return repo

-- catch exceptions
-- fatal: destination path 'packages' already exists and is not an empty directory.
clone repo = do   
               Directory.createDirectoryIfMissing True (Type.local repo)
               current <- getCurrentDirectory
               Directory.setCurrentDirectory (Type.local repo)
               runCommand $ concat ["git clone ", (Type.remote repo)] -- ADD --quiet?
               Directory.setCurrentDirectory current
               return repo

pull repo = do    
               current <- getCurrentDirectory
               Directory.setCurrentDirectory (concat [Type.local repo, [pathSeparator], Type.name repo])
               runCommand $ concat ["git pull ", Type.remote repo] -- ADD --quiet?
               Directory.setCurrentDirectory current
               return repo
                                     


                                     