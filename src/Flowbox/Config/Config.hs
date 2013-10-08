{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Config.Config where

import           Flowbox.Prelude            hiding (read, error)
import qualified Data.Configurator          as Cfg
import qualified System.Environment         as Env
import           Data.Maybe                   (fromJust)
import           Control.Applicative          
import           Flowbox.System.Log.Logger    
import           Control.Exception            
import           Control.Monad                
import           Control.Monad.Trans.Either   
import           Control.Monad.IO.Class       



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
ffs = "FLOWBOX_FS"


load :: MonadIO m => m (Either IOException Config)
load = runEitherT $ do
    logger debug "Loading Flowbox configuration"
    env   <- liftIO.try $ Env.getEnv ffs
    when (isLeft env) $ logger debug $ "Environment variable '" ++ ffs ++ "' not defined."
    cpath <- hoistEither env
    cfgf  <- liftIO $ Cfg.load [Cfg.Required $ cpath ++ "/config/flowbox.config"]
    let read name = do field <- (liftIO . try) (fromJust <$> Cfg.lookup cfgf name :: IO String)
                       when (isLeft field) $ logger debug $ "Environment variable '" ++ ffs ++ "' not defined."
                       hoistEither field
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