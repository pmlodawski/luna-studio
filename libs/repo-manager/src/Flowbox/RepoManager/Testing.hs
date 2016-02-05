{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Flowbox.RepoManager.Testing where

import qualified Control.Exception.Base                   as Exception
import           Control.Monad                            (when)
import qualified Data.ByteString.Lazy                     as BSL
import qualified Data.Configurator                        as Configurator
import qualified Data.Configurator.Types                  as Configurator
import qualified Data.List.Split                          as Split
import qualified Data.Maybe                               as Maybe
import qualified Data.Set                                 as Set
import qualified Data.Text                                as Text
import           Data.Text.Lens                           (packed)
import qualified Data.Version                             as Version
import           Flowbox.Prelude                          hiding (Equality)
import           Flowbox.RepoManager.Data.Dependency      as Dependency
import           Flowbox.RepoManager.Data.Package.Package
import           Flowbox.RepoManager.Data.Version
import qualified Flowbox.System.Log.Logger                as Logger
import           GHC.Generics                             (Generic)
import qualified Network.HTTP.Conduit                     as Conduit
import qualified Network.URI                              as URI
import qualified System.Directory                         as Directory
import qualified System.Exit                              as Exit
import qualified System.FilePath                          as FilePath
import qualified System.Process                           as Process
import qualified Text.ParserCombinators.ReadP             as ReadP

type Command = String

--instance Binary.Binary Package
--instance Binary.Binary Dep
--instance Binary.Binary Equality
--instance Binary.Binary Constraint

--deriving instance Generic Version.Version
--deriving instance Generic URI.URI

--instance Binary.Binary Version.Version
--instance Binary.Binary URI.URI

--instance Eq Package where
--    p1 == p2 = p1 ^. name == p2 ^. name && p1 ^. version == p2 ^. version

--instance Ord Package where
--    compare p1 p2 = compare (p1 ^. name) (p2 ^. name) ++
--                    compare (p1 ^. version) (p2 ^. version)

logger :: Logger.LoggerIO
logger = Logger.getLoggerIO "Flowbox.Config.Config"

installPackage :: Package -> IO ()
installPackage package = undefined --do --downloadSource package
                            --runScript (package ^. install) `Exception.catch` \(e :: Exit.ExitCode) -> putStrLn "installation failed"

--createDB :: FilePath -> Set.Set Package
--createDB path = undefined

findBuildFilesInDir :: FilePath -> IO [FilePath]
findBuildFilesInDir dir = do files <- Directory.getDirectoryContents dir
                             let scripts = filter (\x -> FilePath.takeExtension x == ".config") files
                             return scripts


