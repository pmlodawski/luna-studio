
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
import qualified Data.Map                                         as Map
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
import qualified Generated.Proto.Exception                        as Exception
import qualified Generated.Proto.Request                          as Request
import           Generated.Proto.Request                            (Request)
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


zmqRunScript :: (Reflections.ReflectDescriptor r, WireMessage.Wire r)
             => Extensions.Key Maybe Response r -> Script r -> ZMQ3.ZMQ z ByteString
zmqRunScript rspkey s = do
    e <- ZMQ3.liftIO $ runEitherT s
    let r = Response ResponseType.Exception $ Extensions.ExtField Map.empty
    Proto.messagePut <$> case e of
        Left  m -> do loggerIO error m
                      let exc = Exception $ Just $ Proto.uFromString m
                      return $ Extensions.putExt Exception.rsp (Just exc) r
        Right a ->    return $ Extensions.putExt rspkey        (Just a  ) r


call :: (WireMessage.Wire r, Reflections.ReflectDescriptor r)
     => Request -> h ->  (h -> arg -> Script r)
     -> Extensions.Key Maybe Request arg 
     -> Extensions.Key Maybe Response r 
     -> ZMQ3.ZMQ z ByteString
call request handler method reqkey rspkey = case Extensions.getExt reqkey request of 
    Right (Just args) -> zmqRunScript rspkey $ method handler args
    Left   e'         -> fail $ "Error while getting extension: " ++ e'
    _                 -> fail $ "Error while getting extension"


selectCall :: Handler h => h -> ByteString -> ZMQ3.ZMQ z ByteString
selectCall handler encoded_request = case Proto.messageGet encoded_request of
    Left   e           -> fail $ "Error while decoding request: " ++ e
    Right (request, _) -> case Request.method request of 
        Method.LS    -> call request handler Handler.ls    LS.req    LS.rsp    
        Method.Stat  -> call request handler Handler.stat  Stat.req  Stat.rsp  
        Method.MkDir -> call request handler Handler.mkdir MkDir.req MkDir.rsp 
        Method.Touch -> call request handler Handler.touch Touch.req Touch.rsp 
        Method.RM    -> call request handler Handler.rm    RM.req    RM.rsp    
        Method.CP    -> call request handler Handler.cp    CP.req    CP.rsp    
        Method.MV    -> call request handler Handler.mv    MV.req    MV.rsp    

        Method.Projects      -> call request handler Handler.projects      Projects.req      Projects.rsp      
        Method.ProjectByID   -> call request handler Handler.projectByID   ProjectByID.req   ProjectByID.rsp   
        Method.CreateProject -> call request handler Handler.createProject CreateProject.req CreateProject.rsp 
        Method.OpenProject   -> call request handler Handler.openProject   OpenProject.req   OpenProject.rsp   
        Method.UpdateProject -> call request handler Handler.updateProject UpdateProject.req UpdateProject.rsp
        Method.CloseProject  -> call request handler Handler.closeProject  CloseProject.req  CloseProject.rsp  
        Method.StoreProject  -> call request handler Handler.storeProject  StoreProject.req  StoreProject.rsp  

        Method.Initialize -> call request handler Handler.initialize Initialize.req Initialize.rsp
        Method.Ping       -> call request handler Handler.ping       Ping.req       Ping.rsp      
        Method.Dump       -> call request handler Handler.dump       Dump.req       Dump.rsp      
        Method.Shutdown   -> call request handler Handler.shutdown   Shutdown.req   Shutdown.rsp  


process :: (Handler h, ZMQ3.Receiver t, ZMQ3.Sender t) => ZMQ3.Socket z t -> h -> ZMQ3.ZMQ z ()
process socket handler = do
    ZMQ3.liftIO $ loggerIO debug "method processing started"
    encoded_request  <- ByteString.fromStrict <$> ZMQ3.receive socket
    encoded_response <- ByteString.toStrict   <$> selectCall handler encoded_request
    ZMQ3.send socket [] encoded_response
