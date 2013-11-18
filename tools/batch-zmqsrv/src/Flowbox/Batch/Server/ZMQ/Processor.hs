
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Batch.Server.ZMQ.Processor where

import           Control.Applicative                                
import qualified Data.ByteString.Lazy                             as ByteString
import           Data.ByteString.Lazy                               (ByteString)
import qualified System.ZMQ3.Monadic                              as ZMQ3
import qualified Text.ProtocolBuffers                             as Proto
import qualified Text.ProtocolBuffers.Basic                       as Proto
import qualified Text.ProtocolBuffers.Extensions                  as Extensions
import qualified Text.ProtocolBuffers.Reflections                 as Reflections
import qualified Text.ProtocolBuffers.WireMessage                 as WireMessage

import           Flowbox.Prelude                                  hiding (error)
import qualified Flowbox.Batch.Server.ZMQ.Handlers.Handler        as Handler
import           Flowbox.Batch.Server.ZMQ.Handlers.Handler          (Handler)
import           Flowbox.Control.Error                              
import           Flowbox.System.Log.Logger                          
import           Generated.Proto.Exception                          (Exception(Exception))
import qualified Generated.Proto.Request                          as Request
import           Generated.Proto.Request                            (Request(Request))
import qualified Generated.Proto.Request.Method                   as Method
import           Generated.Proto.Response                           (Response(Response))
import qualified Generated.Proto.Response.Type                    as ResponseType
import qualified Generated.Proto.FileSystemAPI.LS.Args            as LS
import qualified Generated.Proto.FileSystemAPI.LS.Result          as LS
import qualified Generated.Proto.FileSystemAPI.Stat.Args          as Stat
import qualified Generated.Proto.FileSystemAPI.Stat.Result        as Stat
import qualified Generated.Proto.FileSystemAPI.MkDir.Args         as MkDir
import qualified Generated.Proto.FileSystemAPI.MkDir.Result       as MkDir
import qualified Generated.Proto.FileSystemAPI.Touch.Args         as Touch
import qualified Generated.Proto.FileSystemAPI.Touch.Result       as Touch
import qualified Generated.Proto.FileSystemAPI.RM.Args            as RM
import qualified Generated.Proto.FileSystemAPI.RM.Result          as RM
import qualified Generated.Proto.FileSystemAPI.CP.Args            as CP
import qualified Generated.Proto.FileSystemAPI.CP.Result          as CP
import qualified Generated.Proto.FileSystemAPI.MV.Args            as MV
import qualified Generated.Proto.FileSystemAPI.MV.Result          as MV
import qualified Generated.Proto.ProjectAPI.Projects.Args         as Projects
import qualified Generated.Proto.ProjectAPI.Projects.Result       as Projects
import qualified Generated.Proto.ProjectAPI.ProjectByID.Args      as ProjectByID
import qualified Generated.Proto.ProjectAPI.ProjectByID.Result    as ProjectByID
import qualified Generated.Proto.ProjectAPI.CreateProject.Args    as CreateProject
import qualified Generated.Proto.ProjectAPI.CreateProject.Result  as CreateProject
import qualified Generated.Proto.ProjectAPI.OpenProject.Args      as OpenProject
import qualified Generated.Proto.ProjectAPI.OpenProject.Result    as OpenProject
import qualified Generated.Proto.ProjectAPI.UpdateProject.Args    as UpdateProject
import qualified Generated.Proto.ProjectAPI.UpdateProject.Result  as UpdateProject
import qualified Generated.Proto.ProjectAPI.CloseProject.Args     as CloseProject
import qualified Generated.Proto.ProjectAPI.CloseProject.Result   as CloseProject
import qualified Generated.Proto.ProjectAPI.StoreProject.Args     as StoreProject
import qualified Generated.Proto.ProjectAPI.StoreProject.Result   as StoreProject
import qualified Generated.Proto.MaintenanceAPI.Initialize.Args   as Initialize
import qualified Generated.Proto.MaintenanceAPI.Initialize.Result as Initialize
import qualified Generated.Proto.MaintenanceAPI.Ping.Args         as Ping
import qualified Generated.Proto.MaintenanceAPI.Ping.Result       as Ping
import qualified Generated.Proto.MaintenanceAPI.Dump.Args         as Dump
import qualified Generated.Proto.MaintenanceAPI.Dump.Result       as Dump
import qualified Generated.Proto.MaintenanceAPI.Shutdown.Args     as Shutdown
import qualified Generated.Proto.MaintenanceAPI.Shutdown.Result   as Shutdown



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ.Processor"


zmqRunScript :: (Reflections.ReflectDescriptor a, WireMessage.Wire a)
             => Script a -> ZMQ3.ZMQ z ByteString
zmqRunScript s = do
    e <- ZMQ3.liftIO $ runEitherT s
    case e of
        Left  m -> do loggerIO error m
                      let t  = Proto.messagePut $ Response ResponseType.Exception
                          ex = Proto.messagePut $ Exception $ Just $ Proto.uFromString m
                      return $ ByteString.append t ex
        Right a -> do let t = Proto.messagePut $ Response ResponseType.Result
                          r = Proto.messagePut a
                      return $ ByteString.append t r


call :: (WireMessage.Wire a, Reflections.ReflectDescriptor a)
     => Request -> h -> Extensions.Key Maybe Request v -> (h -> v -> Script a) -> ZMQ3.ZMQ z ByteString
call request handler key method = case Extensions.getExt key request of 
    Right (Just args) -> zmqRunScript $ method handler args
    Left   e'         -> fail $ "Error while getting extension: " ++ e'
    _                 -> fail $ "Error while getting extension"


selectCall :: Handler h => h -> ByteString -> ZMQ3.ZMQ z ByteString
selectCall handler encoded_request = case Proto.messageGet encoded_request of
    Left   e           -> fail $ "Error while decoding request: " ++ e
    Right (request, _) -> case Request.method request of 
        Method.LS    -> call request handler LS.ext    Handler.ls
        Method.Stat  -> call request handler Stat.ext  Handler.stat
        Method.MkDir -> call request handler MkDir.ext Handler.mkdir
        Method.Touch -> call request handler Touch.ext Handler.touch
        Method.RM    -> call request handler RM.ext    Handler.rm
        Method.CP    -> call request handler CP.ext    Handler.cp
        Method.MV    -> call request handler MV.ext    Handler.mv

        Method.Projects      -> call request handler Projects.ext      Handler.projects
        Method.ProjectByID   -> call request handler ProjectByID.ext   Handler.projectByID
        Method.CreateProject -> call request handler CreateProject.ext Handler.createProject
        Method.OpenProject   -> call request handler OpenProject.ext   Handler.openProject
        Method.UpdateProject -> call request handler UpdateProject.ext Handler.updateProject
        Method.CloseProject  -> call request handler CloseProject.ext  Handler.closeProject
        Method.StoreProject  -> call request handler StoreProject.ext  Handler.storeProject

        Method.Initialize -> call request handler Initialize.ext Handler.initialize
        Method.Ping       -> call request handler Ping.ext       Handler.ping
        Method.Dump       -> call request handler Dump.ext       Handler.dump
        Method.Shutdown   -> call request handler Shutdown.ext   Handler.shutdown


process :: (Handler h, ZMQ3.Receiver t, ZMQ3.Sender t) => ZMQ3.Socket z t -> h -> ZMQ3.ZMQ z ()
process socket handler = do
    ZMQ3.liftIO $ loggerIO debug "method processing started"
    encoded_request  <- ByteString.fromStrict <$> ZMQ3.receive socket
    encoded_response <- ByteString.toStrict   <$> selectCall handler encoded_request
    ZMQ3.send socket [] encoded_response
