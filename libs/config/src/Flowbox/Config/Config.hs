{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Config.Config where

import           Control.Applicative         
import qualified Control.Exception         as Exception
import qualified Data.Configurator         as Configurator
import qualified System.Environment        as Env

import           Flowbox.Prelude           hiding (read, error)
import           Flowbox.System.Log.Logger   
import qualified Prelude                   as Prelude


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Config.Config"


data Config = Config      { global     :: Section
                          , local      :: Section
                          , templates  :: Section
                          , config     :: Section
                          , tools      :: Section
                          , wrappers   :: Section
                          , thirdparty :: Section
                          }
            deriving (Show)
                          
data Section = Global     { path      :: String
                          , conf      :: String
                          , bin       :: String
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
             | Ghc        { version   :: String
                          , path      :: String
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


ffs :: String
ffs = "FFS"


load :: IO Config
load = do
    logger debug "Loading Flowbox configuration"
    cpath <- Exception.onException (Env.getEnv ffs)
           $ logger error ("Environment variable '" ++ ffs ++ "' not defined.")

    cfgFile <- Configurator.load [Configurator.Required $ cpath ++ "/config/flowbox.config"]

    let read name = Exception.onException (fromJust =<< (Configurator.lookup cfgFile name :: IO (Maybe String)))
                  $ logger error ("Error reading config variable '" ++ show name)

    Config <$> ( Global <$> read "global.path"
                        <*> read "global.conf"
                        <*> read "global.bin"
               )
           <*> ( Local  <$> read "local.home"
                        <*> read "local.path"
                        <*> read "local.conf"
                        <*> read "local.pkgDb"
                        <*> read "local.cabal"
               )
           <*> ( Templates <$> read "templates.path"
                           <*> read "templates.cabal"
               )
           <*> ( Cfg <$> read "config.flowbox"
                     <*> read "config.cabal"
               )
           <*> ( Tools <$> read "tools.path"
                       <*> read "tools.lunac"
                       <*> read "tools.batchSrv"
               )
           <*> ( Wrappers <$> read "wrappers.path"
                          <*> read "wrappers.ghc"
                          <*> read "wrappers.ghcPkg"
                          <*> read "wrappers.hsc2hs"
                          <*> read "wrappers.cabal"
               )
           <*> ( ThirdParty <$> read "thirdparty.path"
                            <*> ( Ghc <$> read "thirdparty.ghc.version"
                                      <*> read "thirdparty.ghc.path"
                                      <*> read "thirdparty.ghc.topDir"
                                      <*> read "thirdparty.ghc.pkgConf"
                                      <*> read "thirdparty.ghc.ghcBin"
                                      <*> read "thirdparty.ghc.ghcPkgBin"
                                      <*> read "thirdparty.ghc.hsc2hsBin"
                                )
                            <*> ( Cabal <$> read "thirdparty.cabal.path"
                                        <*> read "thirdparty.cabal.binDir"
                                        <*> read "thirdparty.cabal.cabalBin"
                                )
               )

-- TODO[wd]: (?) Lunac powinien czytac config i jezli nie da sie go odczytac (np zmienna srodowiskowa nie istnieje, powinien zalozyc, ze zyje w $HOME/.flowbox - defaultowy config?)