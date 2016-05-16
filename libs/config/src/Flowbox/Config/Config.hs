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
import qualified Control.Exception         as Exception
import qualified Data.Configurator         as Configurator
import qualified System.Environment        as Env

import           Flowbox.Prelude           hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO $moduleName


data Config = Config      { root      :: Section
                          , projects  :: Section
                          , bus       :: Section
                          }
            deriving (Show)

data Section = Root       { path :: String
                          , conf :: String
                          , bin  :: String
                          }
             | Projects   { projectRoot  :: String
                          }
             | Bus        { serverControlEndPoint :: String
                          , serverPullEndPoint    :: String
                          , serverPubEndPoint     :: String
                          , clientControlEndPoint :: String
                          , clientPullEndPoint    :: String
                          , clientPubEndPoint     :: String
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

    let readConf name = Exception.onException (fromJustM =<< (Configurator.lookup cfgFile name :: IO (Maybe String)))
                      $ logger error ("Error reading config variable '" ++ show name)

    --let readConfDefault val name = Configurator.lookupDefault val cfgFile name

    Config <$> ( Root <$> readConf "root.path"
                      <*> readConf "root.conf"
                      <*> readConf "root.bin"
               )
           <*> ( Projects <$> readConf "projects.projectRoot"
               )
           <*> ( Bus      <$> readConf "bus.serverControlEndPoint"
                          <*> readConf "bus.serverPullEndPoint"
                          <*> readConf "bus.serverPubEndPoint"
                          <*> readConf "bus.clientControlEndPoint"
                          <*> readConf "bus.clientPullEndPoint"
                          <*> readConf "bus.clientPubEndPoint"
               )

-- TODO[wd]: (?) Lunac powinien czytac config i jezli nie da sie go odczytac (np zmienna srodowiskowa nie istnieje, powinien zalozyc, ze zyje w $HOME/.flowbox - defaultowy config?)
