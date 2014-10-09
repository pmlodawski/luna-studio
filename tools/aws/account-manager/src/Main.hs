---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Database.PostgreSQL.Simple as PSQL

import           Flowbox.AccountManager.Cmd             (Cmd)
import qualified Flowbox.AccountManager.Cmd             as Cmd
import qualified Flowbox.AccountManager.Config          as Config
import qualified Flowbox.AccountManager.Context         as Context
import qualified Flowbox.AccountManager.Handler.Handler as Handler
import qualified Flowbox.AccountManager.InstanceMonitor as InstanceMonitor
import qualified Flowbox.AccountManager.Version         as Version
import qualified Flowbox.AWS.Region                     as Region
import qualified Flowbox.Control.Concurrent             as Concurrent
import           Flowbox.Options.Applicative            hiding (info)
import qualified Flowbox.Options.Applicative            as Opt
import           Flowbox.Prelude                        hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.ZMQ.RPC.Server.Server          as RPC


rootLogger :: Logger
rootLogger = getLogger ""


logger :: LoggerIO
logger = getLoggerIO $(moduleName)


parser :: Parser Cmd
parser = Opt.flag' Cmd.Version (long "version" <> hidden)
       <|> Cmd.Serve
           <$> strOption  (long "address" <> short 'a' <> value (def Config.address) <> metavar "address" <> help ("Server address (default is " ++ (def Config.address) ++ ")"))

           <*> strOption  (long "region"  <> short 'r' <> value (def Config.region)  <> metavar "region"  <> help "Database connect address")

           <*> strOption  (long "dbAddress"  <> metavar "address"  <> help "Database connect address")
           <*> option     (long "dbPort"     <> metavar "port"     <> help "Database connect port")
           <*> option     (long "dbUser"     <> metavar "user"     <> help "Database connect user")
           <*> option     (long "dbPassword" <> metavar "password" <> value "" <> help "Database connect password")
           <*> option     (long "database"   <> metavar "database" <> help "Database name")

           <*> optIntFlag (Just "verbose") 'v' 2 3 "Verbose level (level range is 0-5, default level is 3)"
           <*> switch     (long "no-color" <> help "Disable color output" )


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
        let connectionInfo = PSQL.ConnectInfo (Cmd.dbAddress  cmd)
                                              (fromIntegral $ Cmd.dbPort cmd)
                                              (Cmd.dbUser     cmd)
                                              (Cmd.dbPassword cmd)
                                              (Cmd.database   cmd)
            region = Region.fromString $ Cmd.region cmd
        ctx <- Context.mk region connectionInfo
        _ <- Concurrent.forkIO $ InstanceMonitor.run ctx
        RPC.run 16 (Cmd.address cmd) (Handler.handler ctx) -- TODO [PM] hardcoded number of workers
