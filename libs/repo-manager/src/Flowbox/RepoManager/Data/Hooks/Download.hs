module Flowbox.RepoManager.Data.Hooks.Download where

import Flowbox.Prelude
import qualified Flowbox.RepoManager.Data.Repository      as Repository
import qualified Flowbox.RepoManager.Data.Package.Package as Package
import qualified Flowbox.RepoManager.Data.RepoConfig      as RepoConfig
import qualified Data.ByteString.Lazy                     as BSL
import qualified Network.HTTP.Conduit                     as Conduit
import qualified System.FilePath                          as FilePath

download :: Repository.Repository a -> Package.Package -> IO FilePath
download repo package = do let url      = show $ package ^. Package.source
                               path     = Repository.config repo ^. RepoConfig.downloadPath
                               filePath = FilePath.joinPath [path, FilePath.takeFileName url]
                           downloadedFile <- Conduit.simpleHttp url
                           BSL.writeFile filePath downloadedFile
                           return filePath