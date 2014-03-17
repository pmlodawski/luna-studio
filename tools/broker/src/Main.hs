---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Main where

import qualified Control.Concurrent as Concurrent

import           Flowbox.Broker.Cmd                  (Cmd)
import qualified Flowbox.Broker.Cmd                  as Cmd
import qualified Flowbox.Broker.Proxy                as Proxy
import qualified Flowbox.Broker.Version              as Version
import qualified Flowbox.Bus.Control.BusCtx          as BusCtx
import qualified Flowbox.Bus.Control.Handler.Handler as Handler
import qualified Flowbox.Bus.EndPoint                as EP
import qualified Flowbox.Config.Config               as Config
import           Flowbox.Options.Applicative         hiding (info)
import qualified Flowbox.Options.Applicative         as Opt
import           Flowbox.Prelude                     hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.ZMQ.RPC.Server              as RPC



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Broker"


parser :: Parser Cmd
parser = Opt.flag' Cmd.Version (long "version" <> hidden)
       <|> Cmd.Serve
           <$> optIntFlag (Just "verbose") 'v' 2 3          "Verbose level (level range is 0-5, default level is 3)"
           <*> switch    ( long "no-color"          <> help "Disable color output" )


opts :: ParserInfo Cmd
opts = Opt.info (helper <*> parser)
                (Opt.fullDesc <> Opt.header (Version.full False))


main :: IO ()
main = execParser opts >>= run


run :: Cmd -> IO ()
run cmd = case cmd of
    Cmd.Version  -> putStrLn (Version.full False) -- TODO [PM] hardcoded numeric = False
    Cmd.Serve {} -> do
        rootLogger setIntLevel $ Cmd.verbose cmd
        endPoints <- EP.serverFromConfig <$> Config.load
        logger info "Starting proxy service"
        _ <- Concurrent.forkIO $ Proxy.run (EP.pullEndPoint endPoints) (EP.pubEndPoint endPoints)
        logger info "Starting control service"
        ctx <- BusCtx.empty
        RPC.run (EP.controlEndPoint endPoints) (Handler.handler ctx)
