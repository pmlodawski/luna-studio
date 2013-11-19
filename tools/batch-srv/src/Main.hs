
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

import           Prelude                                    hiding (error)
import           Control.Monad                                (when)
import qualified Control.Concurrent                         as Concurrent
import qualified Control.Concurrent.MVar                    as MVar
import           Control.Concurrent.MVar                      (MVar)
import qualified Control.Exception                          as Exception
import qualified Data.Text.Lazy                             as Text
import qualified System.Exit                                as Exit
import qualified Thrift.Protocol.Binary                     as TProtocol
import           Thrift.Protocol.Binary                       (Protocol)
import           Thrift.Transport                             (Transport)

import qualified Batch                                      as TBatch
import           Batch_Iface                                  
import qualified Flowbox.Batch.Server.Cmd                   as Cmd
import           Flowbox.Batch.Server.Cmd                     (Cmd)
import qualified Flowbox.Batch.Server.Handlers.BatchHandler as BatchHandler
import qualified Flowbox.Batch.Server.Server                as Server
import qualified Flowbox.Batch.Server.Version               as Version
import qualified Flowbox.Options.Applicative                as Opt
import           Flowbox.Options.Applicative                hiding (info)
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
           <$> strOption ( long "addres"  <> short 'a' <> value defaultAddress       <> metavar "address" <> help "Server address"       )
           <*> strOption ( long "port"    <> short 'p' <> (value $ show defaultPort) <> metavar "port"    <> help "Server port"          )
           <*> optIntFlag (Just "verbose") 'v' 2 3                                 "Verbose level (level range is 0-5, default level is 3)"
           <*> switch    ( long "no-color"                                                                <> help "Disable color output" )
           <*> switch    ( long "shutdown-with-client" <> hidden                                                                         )


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
        quitmutex <- MVar.newEmptyMVar
        _ <- Concurrent.forkIO $ Exception.handle 
            (\(e :: Exception.SomeException) -> do loggerIO error $ "Server run failure: " ++ show e
                                                   MVar.putMVar quitmutex True) 
            (serve cmd quitmutex)
        waitForQuit quitmutex


serve :: Cmd -> MVar Bool -> IO ()
serve cmd quitmutex = do
    loggerIO info "Starting the server"
    handler <- BatchHandler.empty
    _ <- Server.runSingleConnectionServer Server.accepter handler (processCommand quitmutex) cmd
    return ()


processCommand :: (Protocol iprot, Protocol oprot, Transport itransp, Transport otransp, Batch_Iface batch)
               => MVar Bool -> batch -> (iprot itransp, oprot otransp) -> IO Bool
processCommand quitmutex handler (iprot, oprot) = do
    (name, typ, seqid) <- TProtocol.readMessageBegin iprot
    TBatch.proc_ handler (iprot,oprot) (name,typ,seqid)
    when (name == Text.pack "shutdown") (MVar.putMVar quitmutex True)
    return True


waitForQuit :: MVar t -> IO b
waitForQuit quitmutex = do
    _ <- MVar.takeMVar quitmutex
    loggerIO warning "shutting down..."
    Exit.exitSuccess
