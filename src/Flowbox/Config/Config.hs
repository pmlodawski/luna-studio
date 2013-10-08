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


data Config = Config { fs         :: Config
                     , wrappers   :: String
                     , thirdparty :: String
                     , config     :: Config
                     , ghc        :: Config
                     , cabalInst  :: Config
                     }

            | FS     { global     :: String
                     , usr        :: String
                     }

            | Conf   { path       :: String
                     , cabal      :: String
                     }

            | GHC    { path       :: String
                     , version    :: String
                     , topDir     :: String
                     , pkgConf    :: String
                     }

            | Cabal  { path       :: String
                     , bin        :: String
                     }
            deriving (Show)


ffs :: String
ffs = "FFS"


load :: MonadIO m => m Config
load = liftIO $ do
    logger debug "Loading Flowbox configuration"
    cpath <- Exception.onException (Env.getEnv ffs)
           $ logger debug ("Environment variable '" ++ ffs ++ "' not defined.")

    cfgFile <- Cfg.load [Cfg.Required $ cpath ++ "/config/flowbox.config"]
    let read name = Exception.onException (Maybe.fromJust <$> Cfg.lookup cfgFile name :: IO String)
                  $ logger debug $ "Environment variable '" ++ ffs ++ "' not defined."

    Config <$> ( FS <$> read "fs.global"
                    <*> read "fs.usr"
               )
           <*> read "wrappers"
           <*> read "thirdparty"
           <*> ( Conf  <$> read "config.path"
                       <*> read "config.cabal"
               )
           <*> ( GHC   <$> read "ghc.path"
                       <*> read "ghc.version"
                       <*> read "ghc.topDir"
                       <*> read "ghc.pkgConf"
               )
           <*> ( Cabal <$> read "cabal.path"
                       <*> read "cabal.bin"
               )

-- TODO[wd]: (?) Lunac powinien czytac config i jezli nie da sie go odczytac (np zmienna srodowiskowa nie istnieje, powinien zalozyc, ze zyje w $HOME/.flowbox - defaultowy config?)