
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Control.Concurrent      as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception       as Exception
import qualified System.Exit             as Exit

import           Flowbox.Batch.Server.Cmd                  (Cmd)
import qualified Flowbox.Batch.Server.Cmd                  as Cmd
import           Flowbox.Batch.Server.Handler.BatchHandler (BatchHandler)
import qualified Flowbox.Batch.Server.Handler.BatchHandler as BatchHandler
import qualified Flowbox.Batch.Server.Transport.TCP.Server as Server
import qualified Flowbox.Batch.Server.Version              as Version
import           Flowbox.Options.Applicative               hiding (info)
import qualified Flowbox.Options.Applicative               as Opt
import           Flowbox.Prelude                           hiding (error)
import           Flowbox.System.Log.Logger



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server"


defaultAddress :: String
defaultAddress = "127.0.0.1"


defaultPort :: Int
defaultPort = 30521


parser :: Parser Cmd
parser = Opt.flag' Cmd.Version (long "version" <> hidden)
       <|> Cmd.Serve
           <$> strOption ( long "address" <> short 'a' <> value defaultAddress <> metavar "address" <> help "Server address"       )
           <*> option    ( long "port"    <> short 'p' <> (value defaultPort)  <> metavar "port"    <> help "Server port"          )
           <*> optIntFlag (Just "verbose") 'v' 2 3          "Verbose level (level range is 0-5, default level is 3)"
           <*> switch    ( long "no-color"          <> help "Disable color output" )
           <*> switch    ( long "shutdown-with-client" <> hidden                                                                   )


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
        handler <- BatchHandler.empty
        _ <- Concurrent.forkIO $ Exception.handle
            (\(e :: Exception.SomeException) -> do loggerIO error $ "Server run failure: " ++ show e
                                                   MVar.putMVar (BatchHandler.quitMutex handler) True)
            (Server.serve cmd handler)
        waitForQuit handler


waitForQuit :: BatchHandler -> IO b
waitForQuit handler = do
    _ <- MVar.takeMVar $ BatchHandler.quitMutex handler
    loggerIO warning "shutting down..."
    Exit.exitSuccess
