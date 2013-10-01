
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
import           Data.Text.Lazy                               (pack)
import           Options.Applicative                        hiding (info)
import qualified Options.Applicative                        as Opt
import qualified System.Exit                                as Exit
import           Thrift.Transport.Handle                      ()
import qualified Thrift.Protocol.Binary                     as TProtocol
import           Thrift.Protocol.Binary                       (Protocol)
import           Thrift.Transport                             (Transport)

import qualified Batch                                      as TBatch
import           Batch_Iface                                  
import qualified Flowbox.Batch.Server.Conf                  as Conf
import           Flowbox.Batch.Server.Conf                    (Conf)
import qualified Flowbox.Batch.Server.Handlers.BatchHandler as BatchHandler
import qualified Flowbox.Batch.Server.Server                as Server
import qualified Flowbox.Data.Version                       as Version
import           Flowbox.Data.Version                         (Version)
import qualified Flowbox.Initializer.Initializer     as Initializer
import           Flowbox.System.Log.Logger                    



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server"


defaultAddress :: String
defaultAddress = "127.0.0.1"


defaultPort :: Int
defaultPort = 30521


version :: Version
version = Version.mk { Version.minor = 1
                     , Version.stage = Version.Alpha
                     }


parser :: Parser Conf
parser = Opt.flag' Conf.Version (long "version" <> hidden)
       <|> Conf.Serve
           <$> strOption ( long "addres"  <> short 'a' <> value defaultAddress       <> metavar "address" <> help "Server address"       )
           <*> strOption ( long "port"    <> short 'p' <> (value $ show defaultPort) <> metavar "port"    <> help "Server port"          )
           <*> switch    ( long "verbose" <> short 'v'                                                    <> help "Verbose"              )
           <*> switch    ( long "debug"   <> short 'd' <> hidden                                                                         )
           <*> switch    ( long "no-color"                                                                <> help "Disable color output" )
           <*> switch    ( long "shutdown-with-client" <> hidden                                                                         )


opts :: ParserInfo Conf
opts = Opt.info (helper <*> parser)
           (Opt.fullDesc
               <> Opt.header show_version
           )


show_version :: String
show_version = "Batch server, version " ++ Version.str version


main :: IO ()
main = execParser opts >>= run


run :: Conf -> IO ()
run conf = case conf of
    Conf.Version {} -> putStrLn show_version
    Conf.Serve   {} -> do
        if Conf.verbose conf
            then rootLogger setLevel INFO
            else return ()
        if Conf.debug conf
            then rootLogger setLevel DEBUG
            else return ()

        quitmutex <- MVar.newEmptyMVar
        _ <- Concurrent.forkIO $ Exception.handle 
            (\(e :: Exception.SomeException) -> do loggerIO error $ "Server run failure: " ++ show e
                                                   MVar.putMVar quitmutex True) 
            (serve conf quitmutex)
        _ <- Concurrent.forkIO $ Initializer.checkedInitialize
        waitForQuit quitmutex


serve :: Conf -> MVar Bool -> IO ()
serve conf quitmutex = do
    handler <- BatchHandler.empty
    loggerIO info "Starting the server"
    _ <- Server.runSingleConnectionServer Server.accepter handler (processCommand quitmutex) conf
    return ()


processCommand :: (Protocol iprot, Protocol oprot, Transport itransp, Transport otransp, Batch_Iface batch)
         => MVar Bool -> batch -> (iprot itransp, oprot otransp) -> IO Bool
processCommand quitmutex handler (iprot, oprot) = do
    (name, typ, seqid) <- TProtocol.readMessageBegin iprot
    TBatch.proc_ handler (iprot,oprot) (name,typ,seqid)
    when (name == pack "shutdown") (MVar.putMVar quitmutex True)
    return True


waitForQuit :: MVar t -> IO b
waitForQuit quitmutex = do
    _ <- MVar.takeMVar quitmutex
    loggerIO warning "shutting down..."
    Exit.exitSuccess
