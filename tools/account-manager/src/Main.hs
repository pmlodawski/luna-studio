---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import           Flowbox.AccountManager.Cmd             (Cmd)
import qualified Flowbox.AccountManager.Cmd             as Cmd
import qualified Flowbox.AccountManager.Config          as Config
import qualified Flowbox.AccountManager.Context         as Context
import qualified Flowbox.AccountManager.Handler.Handler as Handler
import qualified Flowbox.AccountManager.Version         as Version
import           Flowbox.Options.Applicative            hiding (info)
import qualified Flowbox.Options.Applicative            as Opt
import           Flowbox.Prelude                        hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.ZMQ.RPC.Server                 as RPC



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


logger :: LoggerIO
logger = getLoggerIO "Flowbox.AccountManager"


parser :: Parser Cmd
parser = Opt.flag' Cmd.Version (long "version" <> hidden)
       <|> Cmd.Serve
           <$> strOption ( long "address" <> short 'a' <> value Config.defaultAddress <> metavar "address" <> help ("Server address (default is " ++ Config.defaultAddress ++ ")"))
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
        logger info "Starting rpc service"
        ctx <- Context.mk
        RPC.run (Cmd.address cmd) (Handler.handler ctx)
