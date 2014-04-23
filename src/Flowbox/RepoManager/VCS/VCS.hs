module Flowbox.RepoManager.VCS.VCS where

import qualified Data.List       as List
import           Flowbox.Prelude
import qualified Network.URI     as URI

class VCS a where
  localPath  :: a -> FilePath
  remotePath :: a -> URI.URI
  clone      :: a -> IO ()
  pull       :: a -> IO ()
  push       :: a -> IO ()
  remove     :: a -> IO ()
