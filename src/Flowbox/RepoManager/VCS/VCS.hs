module Flowbox.RepoManager.VCS.VCS where

import qualified Data.List       as List
import           Flowbox.Prelude
import qualified Network.URI     as URI

class VCS a where
  name       :: a -> String
  localPath  :: a -> URI.URI
  remotePath :: a -> URI.URI
  clone      :: a -> IO a
  sync       :: a -> IO a
  remove     :: a -> IO ()
