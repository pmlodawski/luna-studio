{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Config.Config where

import           Control.Applicative         
import           Control.Monad.IO.Class      
import qualified Control.Exception         as Exception
import qualified Data.Configurator         as Cfg
import qualified Data.Maybe                as Maybe
import qualified System.Environment        as Env

import           Flowbox.Prelude           hiding (read, error)
import           Flowbox.System.Log.Logger   


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Config.Config"


data Config = Config   { global     :: Config
                       , usr        :: Config
                       , ghc        :: Config
                       , ghcPkg     :: Config
                       , hsc2hs     :: Config
                       , cabal      :: Config
                       }
    
              | Global { path       :: String
                       , conf       :: String
                       , confDir    :: String
                       , binDir     :: String
                       , wrappers   :: String
                       , tools      :: String
                       , thirdparty :: String
                       }
             
              | Usr    { path       :: String
                       , pkgDb      :: String
                       }
             
              | GHC    { version    :: String
                       , path       :: String
                       , topDir     :: String
                       , exec       :: String
                       , rawExec    :: String
                       , pkgConf    :: String
                       }
    
              | GhcPkg { rawExec    :: String
                       }
    
              | Hsc2Hs { rawExec    :: String
                       }
             
              | Cabal  { path       :: String
                       , exec       :: String
                       , rawExec    :: String
                       , binDir     :: String
                       , usrDir     :: String
                       , confT      :: String
                       , conf       :: String
                       }
              deriving (Show)


ffs :: String
ffs = "FFS"


load :: MonadIO m => m Config
load = liftIO $ do
    logger debug "Loading Flowbox configuration"
    cpath <- Exception.onException (Env.getEnv ffs)
           $ logger error ("Environment variable '" ++ ffs ++ "' not defined.")

    cfgFile <- Cfg.load [Cfg.Required $ cpath ++ "/config/flowbox.config"]
    let read name = Exception.onException (Maybe.fromJust <$> Cfg.lookup cfgFile name :: IO String)
                  $ logger error $ "Environment variable '" ++ ffs ++ "' not defined."

    Config <$> ( Global <$> read "global.path"
                        <*> read "global.conf"
                        <*> read "global.confDir"
                        <*> read "global.binDir"
                        <*> read "global.wrappers"
                        <*> read "global.tools"
                        <*> read "global.thirdparty"
               )
           <*> ( Usr    <$> read "usr.path"
                        <*> read "usr.pkgDb"
               )
           <*> ( GHC    <$> read "ghc.version"
                        <*> read "ghc.path"
                        <*> read "ghc.topDir"
                        <*> read "ghc.exec"
                        <*> read "ghc.rawExec"
                        <*> read "ghc.pkgConf"
               ) 
           <*> ( GhcPkg <$> read "ghcPkg.rawExec"
               )
           <*> ( Hsc2Hs <$> read "hsc2hs.rawExec"
               )
           <*> ( Cabal  <$> read "cabal.path"
                        <*> read "cabal.exec"
                        <*> read "cabal.rawExec"
                        <*> read "cabal.binDir"
                        <*> read "cabal.usrDir"
                        <*> read "cabal.confT"
                        <*> read "cabal.conf"
               )

-- TODO[wd]: (?) Lunac powinien czytac config i jezli nie da sie go odczytac (np zmienna srodowiskowa nie istnieje, powinien zalozyc, ze zyje w $HOME/.flowbox - defaultowy config?)