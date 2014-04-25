{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Flowbox.RepoManager.Testing where

import           Flowbox.Prelude hiding (Equality)
import           Flowbox.RepoManager.Data.Dependency as Dependency
import qualified System.Directory as Directory
import qualified Data.Version as Version
import qualified Network.URI  as URI
import qualified System.FilePath as FilePath
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Set as Set
import qualified Flowbox.System.Log.Logger as Logger
import qualified Data.Maybe as Maybe
import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator
import qualified Data.List.Split as Split
import qualified Control.Exception.Base as Exception
import qualified Data.Text as Text
import qualified System.Process as Process
import qualified System.Exit as Exit
import           Flowbox.RepoManager.Data.Package.Package
import           Flowbox.RepoManager.Data.Version
import           Control.Monad (when)
import           GHC.Generics (Generic)
import           Data.Text.Lens (packed)
import qualified Network.HTTP.Conduit as Conduit
import qualified Data.ByteString.Lazy as BSL

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
installPackage package = do downloadSource package
                            runScript (package ^. install) `Exception.catch` \(e :: Exit.ExitCode) -> putStrLn "installation failed"

runScript :: [Command] -> IO ()
runScript []     = return ()
runScript (c:cs) = do returnCode <- Process.system c
                      when (isFailure returnCode) $ Exception.throwIO returnCode
                      runScript cs

isFailure :: Exit.ExitCode -> Bool
isFailure (Exit.ExitFailure _) = True
isFailure _                    = False

downloadSource :: Package -> IO ()
downloadSource package = do let url = show $ package ^. source
                            downloaded <- Conduit.simpleHttp url
                            BSL.writeFile (FilePath.joinPath ["tmp", FilePath.takeFileName url]) downloaded

--createDB :: FilePath -> Set.Set Package
--createDB path = undefined

findBuildFilesInDir :: FilePath -> IO [FilePath]
findBuildFilesInDir dir = do files <- Directory.getDirectoryContents dir
                             let scripts = filter (\x -> FilePath.takeExtension x == ".config") files
                             return scripts


