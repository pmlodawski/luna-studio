{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Config.Config where

import           Control.Applicative         
import qualified Control.Exception         as Exception
import qualified Data.Configurator         as Configurator
import qualified System.Environment        as Env

import           Flowbox.Prelude           hiding (error)
import qualified Flowbox.Data.Version      as Version
import           Flowbox.Data.Version        (Version)
import           Flowbox.System.Log.Logger   
import qualified Prelude                   as Prelude


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Config.Config"


data Config = Config      { version    :: Version
                          , ffs        :: Section
                          , global     :: Section
                          , local      :: Section
                          , templates  :: Section
                          , config     :: Section
                          , tools      :: Section
                          , wrappers   :: Section
                          , thirdparty :: Section
                          }
            deriving (Show)
                          
data Section = FFS        { path      :: String
                          , conf      :: String
                          , bin       :: String
                          }
             | Global     { path      :: String
                          , bin       :: String
                          , lib       :: String
                          , share     :: String
                          , pkgDb     :: String
                          }
             | Local      { home      :: String
                          , path      :: String
                          , conf      :: String
                          , pkgDb     :: String
                          , cabal     :: String
                          }
             | Templates  { path      :: String
                          , cabal     :: String
                          }
             | Cfg        { flowbox   :: String
                          , cabal     :: String
                          }

             | Tools      { path      :: String
                          , lunac     :: String
                          , batchSrv  :: String
                          }
             | Wrappers   { path      :: String
                          , ghc       :: String
                          , ghcPkg    :: String
                          , hsc2hs    :: String
                          , cabal     :: String
                          }
             | ThirdParty { path      :: String
                          , ghcTP     :: Section
                          , cabalTP   :: Section
                          }
             | Ghc        { version_  :: String
                          , path      :: String
                          , libDir    :: String
                          , topDir    :: String
                          , pkgConf   :: String
                          , ghcBin    :: String
                          , ghcPkgBin :: String
                          , hsc2hsBin :: String
                          }
             | Cabal      { path      :: String
                          , binDir    :: String
                          , cabalBin  :: String
                          }
             deriving (Show)


ffsEnv :: String
ffsEnv = "FFS"


load :: IO Config
load = do
    logger debug "Loading Flowbox configuration"
    cpath <- Exception.onException (Env.getEnv ffsEnv)
           $ logger error ("Flowbox environment not initialized.")
          *> logger error ("Environment variable '" ++ ffsEnv ++ "' not defined.")
          *> logger error ("Please run 'source <FLOWBOX_INSTALL_PATH>/setup' and try again.")

    cfgFile <- Configurator.load [Configurator.Required $ cpath ++ "/config/flowbox.config"]

    let readConf name = Exception.onException (fromJust =<< (Configurator.lookup cfgFile name :: IO (Maybe String)))
                      $ logger error ("Error reading config variable '" ++ show name)

    --let readConfDefault val name = Configurator.lookupDefault val cfgFile name

    Config <$> ( Version.Version <$> (read <$> readConf "info.major")
                                 <*> (read <$> readConf "info.minor")
                                 <*> (read <$> readConf "info.patch")
                                 <*> readConf "info.build"
                                 <*> pure Version.Alpha --readConf "info.stage"
               )
           <*> ( FFS <$> readConf "ffs.path"
                     <*> readConf "ffs.conf"
                     <*> readConf "ffs.bin"
               )
           <*> ( Global <$> readConf "global.path"
                        <*> readConf "global.bin"
                        <*> readConf "global.lib"
                        <*> readConf "global.share"
                        <*> readConf "global.pkgDb"
               )
           <*> ( Local  <$> readConf "local.home"
                        <*> readConf "local.path"
                        <*> readConf "local.conf"
                        <*> readConf "local.pkgDb"
                        <*> readConf "local.cabal"
               )
           <*> ( Templates <$> readConf "templates.path"
                           <*> readConf "templates.cabal"
               )
           <*> ( Cfg <$> readConf "config.flowbox"
                     <*> readConf "config.cabal"
               )
           <*> ( Tools <$> readConf "tools.path"
                       <*> readConf "tools.lunac"
                       <*> readConf "tools.batchSrv"
               )
           <*> ( Wrappers <$> readConf "wrappers.path"
                          <*> readConf "wrappers.ghc"
                          <*> readConf "wrappers.ghcPkg"
                          <*> readConf "wrappers.hsc2hs"
                          <*> readConf "wrappers.cabal"
               )
           <*> ( ThirdParty <$> readConf "thirdparty.path"
                            <*> ( Ghc <$> readConf "thirdparty.ghc.version"
                                      <*> readConf "thirdparty.ghc.path"
                                      <*> readConf "thirdparty.ghc.libDir"
                                      <*> readConf "thirdparty.ghc.topDir"
                                      <*> readConf "thirdparty.ghc.pkgConf"
                                      <*> readConf "thirdparty.ghc.ghcBin"
                                      <*> readConf "thirdparty.ghc.ghcPkgBin"
                                      <*> readConf "thirdparty.ghc.hsc2hsBin"
                                )
                            <*> ( Cabal <$> readConf "thirdparty.cabal.path"
                                        <*> readConf "thirdparty.cabal.binDir"
                                        <*> readConf "thirdparty.cabal.cabalBin"
                                )
               )

-- TODO[wd]: (?) Lunac powinien czytac config i jezli nie da sie go odczytac (np zmienna srodowiskowa nie istnieje, powinien zalozyc, ze zyje w $HOME/.flowbox - defaultowy config?)