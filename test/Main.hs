---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import           Flowbox.Prelude
--import qualified Flowbox.RepoManager.VCS.Git.Git as Git
--import qualified Flowbox.RepoManager.VCS.Type    as VCS
--import qualified Flowbox.RepoManager.VCS.VCS  as VCS

--import qualified           System.Environment              as Environment
import Flowbox.RepoManager.Data.Repository as Repository

main :: IO ()
main = Repository.build "repo/packages"

--main = do
--          args <- Environment.getArgs
--          case args of
--            []         -> Main.clone
--            "update":_ -> Main.update
--            "remove":_ -> Main.remove
--            "clone": _ -> Main.clone
--            _other     -> Main.clone

--create ::  VCS.VCS
--create = Git.createVCS VCS.Git "repo" "packages" "git@github.com:dobry/packages.git"

--clone :: IO ()
--clone = do 
--           _ <- Git.clone create
--           return ()

--update :: IO ()
--update = do
--            _ <- Git.update create
--            return ()

--remove :: IO ()
--remove = do 
--            _ <- Git.remove create
--            return ()

