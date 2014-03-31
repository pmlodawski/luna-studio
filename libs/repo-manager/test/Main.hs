---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import Flowbox.Prelude
import Flowbox.RepoManager.VCS.VCS as VCS
import Flowbox.RepoManager.VCS.Type as VCS.Type

main :: IO ()
main = do 
          --args <- getArgs
          --let (path, ) case args of
          --  ()
          vcs <- VCS.create VCS.Type.Git (VCS.path ["repo"]) "name" "git@github.com:dobry/packages.git"
          VCS.update vcs
          print (VCS.path ["repo"])
          print vcs
          --print exists
          --VCS.createRepository "repo/packages" "git@github.com:dobry/packages.git" 
          --VCS.updateRepository "repo/packages" "git@github.com:dobry/packages.git" 
