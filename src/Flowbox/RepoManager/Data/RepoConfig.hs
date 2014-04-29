module Flowbox.RepoManager.Data.RepoConfig where

import           Flowbox.Prelude
import qualified Flowbox.RepoManager.Data.Repository as Repository
import qualified Flowbox.RepoManager.VCS.VCS         as VCS

data RepoConfig a = RepoConfig { _repository   :: Repository.Repository a
                               , _downloadPath :: FilePath
                               , _makePath     :: FilePath
                               , _installPath  :: FilePath
                               }

repositoryPath :: VCS.VCS a => RepoConfig a -> FilePath
repositoryPath config = VCS.localPath $ Repository.getVCS $ _repository config