
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Control.Concurrent                             as Concurrent
import qualified Control.Concurrent.MVar                        as MVar
import           Control.Concurrent.MVar                          (MVar)
import qualified Control.Exception                              as Exception
import qualified System.Exit                                    as Exit

import           Flowbox.Prelude                                hiding (error)
import qualified Flowbox.Data.Version                           as Version
import           Flowbox.Data.Version                             (Version)
import qualified Flowbox.Batch.Server.CmdArgs                   as CmdArgs
import           Flowbox.Batch.Server.CmdArgs                     (CmdArgs)
import qualified Flowbox.Batch.Server.ZMQ.Server                as Server
import qualified Flowbox.Options.Applicative                    as Opt
import           Flowbox.Options.Applicative                    hiding (info)
import           Flowbox.System.Log.Logger                        
import qualified Flowbox.Batch.Server.ZMQ.Handlers.BatchHandler as BatchHandler



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ"


defaultAddress :: String
defaultAddress = "tcp://*"


defaultPort :: Int
defaultPort = 30521


version :: Version
version = Version.mk { Version.minor = 1
                     , Version.stage = Version.Alpha
                     }


parser :: Parser CmdArgs
parser = Opt.flag' CmdArgs.Version (long "version" <> hidden)
       <|> CmdArgs.Serve
           <$> strOption ( long "addres"  <> short 'a' <> value defaultAddress <> metavar "address" <> help "Server address"       )
           <*> option    ( long "port"    <> short 'p' <> (value defaultPort)  <> metavar "port"    <> help "Server port"          )
           <*> optIntFlag (Just "verbose") 'v' 2 3                           "Verbose level (level range is 0-5, default level is 3)"
           <*> switch    ( long "no-color"                                                          <> help "Disable color output" )
           <*> switch    ( long "shutdown-with-client" <> hidden                                                                   )


opts :: ParserInfo CmdArgs
opts = Opt.info (helper <*> parser)
           (Opt.fullDesc
               <> Opt.header show_version
           )


show_version :: String
show_version = "Batch server, version " ++ Version.str version


main :: IO ()
main = execParser opts >>= run


run :: CmdArgs -> IO ()
run cmd = case cmd of
    CmdArgs.Version {} -> putStrLn show_version
    CmdArgs.Serve   {} -> do
        rootLogger setIntLevel $ CmdArgs.verbose cmd
        quitmutex <- MVar.newEmptyMVar
        _ <- Concurrent.forkIO $ Exception.handle 
            (\(e :: Exception.SomeException) -> do loggerIO error $ "Server run failure: " ++ show e
                                                   MVar.putMVar quitmutex True) 
            (serve cmd quitmutex)
        waitForQuit quitmutex


serve :: CmdArgs -> MVar Bool -> IO ()
serve cmd quitmutex = do
    handler <- BatchHandler.empty
    Server.serve (CmdArgs.address cmd) (CmdArgs.port cmd) handler


waitForQuit :: MVar t -> IO b
waitForQuit quitmutex = do
    _ <- MVar.takeMVar quitmutex
    loggerIO warning "shutting down..."
    Exit.exitSuccess
