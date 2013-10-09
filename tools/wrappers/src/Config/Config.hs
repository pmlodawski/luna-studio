{-# LANGUAGE OverloadedStrings #-}

module Config.Config where

import           Prelude                   hiding (FilePath)
import           System.Environment          
import           Control.Applicative         
import           System.Directory            
import qualified Filesystem.Path.CurrentOS as Path
import           Filesystem.Path.CurrentOS   (FilePath)
import qualified Data.Text                 as T


data Config = Config { wrappersDir   :: FilePath
                     , installDir    :: FilePath
                     , thirdpartyDir :: FilePath
                     , ghcDir        :: FilePath
                     , ghcLibDir     :: FilePath
                     , ghcTopDir     :: FilePath
                     , ghcPkgConf    :: FilePath
                     , userFB        :: FilePath
                     , userPkgDb     :: FilePath
                     , cabalDir      :: FilePath
                     , cabalBinDir   :: FilePath
                     , confDir       :: FilePath
                     , cabalConf     :: FilePath
                     }


get = do
    filePath <- Path.fromText . T.pack <$> getExecutablePath
    home     <- Path.fromText . T.pack <$> getHomeDirectory
    let config = Config { wrappersDir   = Path.parent filePath
                        , installDir    = Path.parent $ wrappersDir config
                        , thirdpartyDir = Path.append (installDir config)    "thirdparty"
                        , confDir       = Path.append (installDir config)    "config"
                        , ghcDir        = Path.append (thirdpartyDir config) "ghc"
                        , cabalDir      = Path.append (thirdpartyDir config) "cabal-install"
                        , ghcLibDir     = Path.append (ghcDir config)        "lib"
                        , ghcTopDir     = Path.append (ghcLibDir config)     "ghc-7.6.3"
                        , ghcPkgConf    = Path.append (ghcTopDir config)     "package.conf.d"
                        , userFB        = Path.append home                   ".flowbox"
                        , userPkgDb     = Path.append (userFB config)        "ghc"
                        , cabalBinDir   = Path.append (cabalDir config)      "bin"
                        , cabalConf     = Path.append (confDir config)       "cabal.config"
                        }
    return config
