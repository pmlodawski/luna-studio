{-# LANGUAGE OverloadedStrings         #-}

module Flowbox.RepoManager.Config.Config where

import           Control.Applicative
import qualified Control.Exception   as Exception
import qualified Data.Configurator   as Configurator
import qualified System.Environment  as Env
import  Data.Text as Text

import           Flowbox.Prelude           hiding (error)
import           Flowbox.System.Log.Logger
import qualified Prelude                   as Prelude
import qualified Data.Configurator.Types (Configured)
import qualified Data.Configurator.Types as Configurator
import Data.Configurator.Types (Value)
import qualified Data.Map as Map
import qualified Flowbox.RepoManager.Data.Item.Item (Item)
import qualified Flowbox.RepoManager.Data.Environment as Environment
--import Data.ConfigFile as Config

logger :: LoggerIO
logger = getLoggerIO "Flowbox.Config.Config"


--data Config = Config      { name :: String
--                          , version :: String
--                          , source :: [(String, String)]
--                          , install :: [String]
--                          , uninstall :: [String]
--                          , dependencies :: [(String, String, String)]
--                          }
--            deriving (Show)

--load :: FilePath -> IO Config
fromString (Configurator.String a) = a
fromList (Configurator.List a) = a
fromNumber (Configurator.Number a) = a
fromBool (Configurator.Bool a) = a

load file = do
                   cfgFile <- Configurator.load [Configurator.Required $ file]

                   let --readConfDef :: String -> IO Value
                       readConf name = Exception.onException (fromJust =<< (Configurator.lookup cfgFile name :: IO (Maybe Value)))
                                     $ logger error ("Error reading config variable '" ++ show name)

                   name <- readConf "name" 
                   version <- readConf "version" 
                   source' <- readConf "source" 
                   install <- readConf "install"
                   uninstall <- readConf "uninstall"
                   dependencies <- readConf "dependencies"
                   print (fromString name)
                   print (fromString version)
                   print (getSource source')
                   print (fromList install)
                   print (fromList uninstall)
                   print (fromList dependencies)

                --install <- 
                --name <- readConf "name"
                --version <- readConf "version"

                --print $ fmap fromNumber (fromList intlist)
                --print $ fromList install
                --cfg <- Configurator.getMap cfgFile
                
                --print $ cfg
                --print (intlist :: Int)


getSource cfgValue = source (fromList cfgValue) Map.empty

source []        architectures = architectures
source (s:sources) architectures = let arch:uri:[] = fromList s
                                       arch' = fromString arch
                                       uri' = fromString uri
                                   in source sources (Map.insert arch' (uriKind uri') architectures)
-- TODO what kind of uri is it?
uriKind uriString = if Text.isPrefixOf "http" uriString 
                       then Environment.Remote $ Text.unpack uriString
                       else Environment.Local $ Text.unpack uriString