{-# LANGUAGE TemplateHaskell #-}
module Flowbox.RepoManager.Data.RepoConfig where

import           Flowbox.Prelude

data RepoConfig = RepoConfig { _downloadPath :: FilePath
                             , _makePath     :: FilePath
                             , _installPath  :: FilePath
                             } deriving Show

makeLenses ''RepoConfig