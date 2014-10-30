---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module Flowbox.Config.Config where

import           Control.Applicative
import qualified Control.Exception   as Exception
import qualified Data.Configurator   as Configurator
import qualified System.Environment  as Env

import Flowbox.Prelude           hiding (error)
import Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


data Config = Config      { root      :: Section
                          , base      :: Section
                          , global    :: Section
                          , local     :: Section
                          , templates :: Section
                          , config    :: Section
                          , tools     :: Section
                          , wrappers  :: Section
                          , bins      :: Section
                          , ghcS      :: Section
                          , bus       :: Section
                          }
            deriving (Show)

data Section = Root       { path :: String
                          , conf :: String
                          , bin  :: String
                          }
             | Base       { path  :: String
                          , bin   :: String
                          , lib   :: String
                          , share :: String
                          , pkgDb :: String
                          }
             | Global     { path  :: String
                          , bin   :: String
                          , lib   :: String
                          , share :: String
                          , pkgDb :: String
                          }
             | Local      { home  :: String
                          , path  :: String
                          , conf  :: String
                          , pkgDb :: String
                          , cabal :: String
                          }
             | Templates  { path  :: String
                          , cabal :: String
                          }
             | Cfg        { flowbox :: String
                          , cabal   :: String
                          }

             | Tools      { path     :: String
                          , lunac    :: String
                          , batchSrv :: String
                          }
             | Wrappers   { path   :: String
                          , ghc    :: String
                          , ghcPkg :: String
                          , hsc2hs :: String
                          , cabal  :: String
                          }
             | Bins       { ghc    :: String
                          , ghcPkg :: String
                          , hsc2hs :: String
                          , cabal  :: String
                          }
             | GHC        { ver    :: String
                          , topDir :: String
                          }
             | Bus        {  serverControlEndPoint :: String
                          ,  serverPullEndPoint    :: String
                          ,  serverPubEndPoint     :: String
                          ,  clientControlEndPoint :: String
                          ,  clientPullEndPoint    :: String
                          ,  clientPubEndPoint     :: String
                          }
             deriving (Show)


lunaRootEnv :: String
lunaRootEnv = "LUNAROOT"


load :: IO Config
load = do
    logger debug "Loading Luna configuration"
    cpath <- Exception.onException (Env.getEnv lunaRootEnv)
           $ logger error ("Luna environment not initialized.")
          *> logger error ("Environment variable '" ++ lunaRootEnv ++ "' not defined.")
          *> logger error ("Please run 'source <LUNA_INSTALL_PATH>/setup' and try again.")

    cfgFile <- Configurator.load [Configurator.Required $ cpath ++ "/config/flowbox.config"]

    let readConf name = Exception.onException (fromJust =<< (Configurator.lookup cfgFile name :: IO (Maybe String)))
                      $ logger error ("Error reading config variable '" ++ show name)

    --let readConfDefault val name = Configurator.lookupDefault val cfgFile name

    Config <$> ( Root <$> readConf "root.path"
                      <*> readConf "root.conf"
                      <*> readConf "root.bin"
               )
           <*> ( Base   <$> readConf "base.path"
                        <*> readConf "base.bin"
                        <*> readConf "base.lib"
                        <*> readConf "base.share"
                        <*> readConf "base.pkgDb"
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
           <*> ( Bins     <$> readConf "bins.ghc"
                          <*> readConf "bins.ghcPkg"
                          <*> readConf "bins.hsc2hs"
                          <*> readConf "bins.cabal"
               )
           <*> ( GHC      <$> readConf "ghc.version"
                          <*> readConf "ghc.topDir"
               )
           <*> ( Bus      <$> readConf "bus.serverControlEndPoint"
                          <*> readConf "bus.serverPullEndPoint"
                          <*> readConf "bus.serverPubEndPoint"
                          <*> readConf "bus.clientControlEndPoint"
                          <*> readConf "bus.clientPullEndPoint"
                          <*> readConf "bus.clientPubEndPoint"
               )

-- TODO[wd]: (?) Lunac powinien czytac config i jezli nie da sie go odczytac (np zmienna srodowiskowa nie istnieje, powinien zalozyc, ze zyje w $HOME/.flowbox - defaultowy config?)
