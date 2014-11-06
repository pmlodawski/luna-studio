---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad.Trans.Either

import qualified Flowbox.Bus.EndPoint                      as EP
import qualified Flowbox.Bus.RPC.Server.Server             as Server
import qualified Flowbox.Config.Config                     as Config
import qualified Flowbox.Control.Concurrent                as Concurrent
import           Flowbox.Control.Error                     (eitherStringToM)
import           Flowbox.Options.Applicative               hiding (info)
import qualified Flowbox.Options.Applicative               as Opt
import           Flowbox.PluginManager.Cmd                 (Cmd)
import qualified Flowbox.PluginManager.Cmd                 as Cmd
import qualified Flowbox.PluginManager.Context             as Context
import qualified Flowbox.PluginManager.Init.Local          as InitLocal
import qualified Flowbox.PluginManager.Init.Remote         as InitRemote
import qualified Flowbox.PluginManager.RPC.Handler.Handler as Handler
import qualified Flowbox.PluginManager.Version             as Version
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO $(moduleName)


rootLogger :: Logger
rootLogger = getLogger ""


parser :: Parser Cmd
parser = Opt.flag' Cmd.Version (long "version" <> hidden)
       <|> Cmd.Run
           <$> strOption  (long "init"   <> short 'i' <> metavar "PATH"   <> value "" <> help "Configuration file with plugins that needs to be run on init")
           <*> strOption  (long "prefix" <> short 'p' <> metavar "PREFIX" <> value "" <> help "Prefix used by this plugin manager (e.g. client, main, etc.")
           <*> optIntFlag (Just "verbose") 'v' 2 3 "Verbose level (level range is 0-5, default level is 3)"
           <*> switch    ( long "no-color"          <> help "Disable color output" )


opts :: ParserInfo Cmd
opts = Opt.info (helper <*> parser)
                (Opt.fullDesc <> Opt.header (Version.full False))


main :: IO ()
main = execParser opts >>= run


run :: Cmd -> IO ()
run cmd = case cmd of
    Cmd.Version -> putStrLn (Version.full False) -- TODO [PM] hardcoded numeric = False
    Cmd.Run initConfig prefix verbose _ -> do
        rootLogger setIntLevel verbose

        cfg <- Config.load

        let busConfig = EP.clientFromConfig cfg

        pluginHandles <- if null initConfig
            then return []
            else do Concurrent.forkIO_ $ runEitherT (InitRemote.init initConfig busConfig) >>= eitherStringToM
                    runEitherT (InitLocal.init initConfig) >>= eitherStringToM

        let ctx = Context.mk cfg pluginHandles

        logger info "Starting rpc server"
        Server.run busConfig ctx (Handler.handlerMap prefix) >>= eitherStringToM

