{-# LANGUAGE TemplateHaskell #-}

module Flowbox.RepoManager.VCS.Git.Git where

import           Data.Bool
import           Flowbox.Prelude
import qualified Flowbox.RepoManager.VCS.VCS  as VCS

import qualified Network.URI as URI
import           System.FilePath ((</>))
import qualified System.Directory as Directory
import           System.Process   (system)
import qualified Flowbox.RepoManager.Utils.Utils as Utils (concatPath)
import qualified Text.Regex.Posix as Regex


data Git = Git { _gitLocalPath  :: FilePath
               , _gitRemotePath :: URI.URI
               } deriving (Show)

makeLenses ''Git

instance VCS.VCS Git where
    localPath  = _gitLocalPath
    remotePath = _gitRemotePath
    clone git  = do Directory.createDirectoryIfMissing True $ show $ VCS.localPath git
                    current <- Directory.getCurrentDirectory
                    Directory.setCurrentDirectory $ show $ VCS.localPath git
                    _ <- system $ "git clone " ++ show (VCS.remotePath git) ++ " " ++ show (VCS.localPath git)
                    Directory.setCurrentDirectory current

    pull git   = do current <- Directory.getCurrentDirectory
                    Directory.setCurrentDirectory $ show $ VCS.localPath git
                    _ <- system $ "git pull"
                    Directory.setCurrentDirectory current

    push git   = do current <- Directory.getCurrentDirectory
                    Directory.setCurrentDirectory $ show $ VCS.localPath git
                    _ <- system $ "git push"
                    Directory.setCurrentDirectory current

    remove git = do let localRepoDirectory = VCS.localPath git
                    _ <- system $ "rm -rf " ++ localRepoDirectory -- HIGHLY UNSAFE
                    return ()


--createVCS :: VCS.Type -> String -> String -> VCS.VCS
--createVCS cls' repoPath remotePath' = VCS.VCS { VCS.cls = cls'
--                                              , VCS.localPath = repoPath
--                                              , VCS.name = getName remotePath'
--                                              , VCS.remotePath = remotePath'
--                                              , VCS.clone = clone
--                                              , VCS.update = update
--                                              , VCS.remove = remove
--                                              }
--    where getName remotePath = head.tail.head $ (remotePath Regex.=~ "([A-Za-z_]*).git$" :: [[String]])

--remove :: VCS.VCS -> IO ProcessHandle
--remove repo = do runCommand $ concat ["rm -fR ", Utils.concatPath [VCS.localPath repo , VCS.name repo]]

--update :: VCS.VCS -> IO VCS.VCS
--update repo = do current <- Directory.getCurrentDirectory
--                 Directory.setCurrentDirectory (VCS.localPath repo)
--                 runCommand $ concat ["git pull --quiet ", VCS.remotePath repo] -- ADD --quiet?
--                 Directory.setCurrentDirectory current
--                 return repo
