
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
import qualified Flowbox.Bus.Defaults                as Defaults
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
           <$> strOption ( long "ctrl-addr" <> short 'c' <> value Defaults.defaultListenCtrlEndPoint <> metavar "endpoint" <> help "Server control endpoint" )
           <*> strOption ( long "pull-addr" <> short 'l' <> value Defaults.defaultListenPullEndPoint <> metavar "endpoint" <> help "Server pull endpoint"    )
           <*> strOption ( long "pub-addr"  <> short 'b' <> value Defaults.defaultListenPubEndPoint  <> metavar "endpoint" <> help "Server publish endpoint" )
           <*> optIntFlag (Just "verbose") 'v' 2 3          "Verbose level (level range is 0-5, default level is 3)"
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
        logger info "Starting proxy service"
        _ <- Concurrent.forkIO $ Proxy.run (Cmd.pullEndPoint cmd) (Cmd.pubEndPoint cmd)
        logger info "Starting control service"
        ctx <- BusCtx.empty
        RPC.run (Cmd.ctrlEndPoint cmd) (Handler.handler ctx)
