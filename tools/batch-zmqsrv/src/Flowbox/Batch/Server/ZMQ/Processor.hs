
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Batch.Server.ZMQ.Processor where

import           Control.Applicative                         
import qualified Data.ByteString.Lazy                      as ByteString
import           Data.ByteString.Lazy                        (ByteString)
import qualified System.ZMQ3.Monadic                       as ZMQ3
import qualified Text.ProtocolBuffers                      as Proto
import qualified Text.ProtocolBuffers.Basic                as Proto
import qualified Text.ProtocolBuffers.Reflections          as Reflections
import qualified Text.ProtocolBuffers.WireMessage          as WireMessage

import           Flowbox.Prelude                           hiding (error)
import qualified Flowbox.Batch.Server.ZMQ.Handlers.Handler as Handler
import           Flowbox.Batch.Server.ZMQ.Handlers.Handler   (Handler)
import           Flowbox.Control.Error                       
import           Flowbox.System.Log.Logger                   
import           Generated.Proto.Exception                   (Exception(Exception))
import qualified Generated.Proto.Method                    as Method
import           Generated.Proto.Method                      (Method(Method))
import qualified Generated.Proto.Method.Name               as MethodName
import           Generated.Proto.Response                    (Response(Response))
import qualified Generated.Proto.Response.Type             as ResponseType


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


call :: (Reflections.ReflectDescriptor a,   WireMessage.Wire a,
         Reflections.ReflectDescriptor msg, WireMessage.Wire msg) 
     => h -> ByteString -> (h -> msg -> Script a) -> ZMQ3.ZMQ z ByteString
call handler encoded_args method = case Proto.messageGet encoded_args of
    Left   e        -> fail $ "Error while decoding arguments from request: " ++ e
    Right (args, _) -> zmqRunScript $ method handler args


selectCall :: Handler h => h -> ByteString -> ZMQ3.ZMQ z ByteString
selectCall handler encoded_request = let 
    (encoded_method, encoded_args) = ByteString.splitAt methodSize encoded_request 
    in case Proto.messageGet encoded_method of
        Left   e          -> fail $ "Error while decoding method from request: " ++ e
        Right (method, _) -> case Method.name method of 
            MethodName.LS         -> call handler encoded_args Handler.ls 
            MethodName.Stat       -> call handler encoded_args Handler.stat
            MethodName.MkDir      -> call handler encoded_args Handler.mkdir
            MethodName.Touch      -> call handler encoded_args Handler.touch
            MethodName.RM         -> call handler encoded_args Handler.rm 
            MethodName.CP         -> call handler encoded_args Handler.cp 
            MethodName.MV         -> call handler encoded_args Handler.mv 

            MethodName.Projects      -> call handler encoded_args Handler.projects 
            MethodName.ProjectByID   -> call handler encoded_args Handler.projectByID 
            MethodName.CreateProject -> call handler encoded_args Handler.createProject 
            MethodName.OpenProject   -> call handler encoded_args Handler.openProject 
            MethodName.UpdateProject -> call handler encoded_args Handler.updateProject 
            MethodName.CloseProject  -> call handler encoded_args Handler.closeProject 
            MethodName.StoreProject  -> call handler encoded_args Handler.storeProject 

            MethodName.Initialize -> call handler encoded_args Handler.initialize 
            MethodName.Ping       -> call handler encoded_args Handler.ping 
            MethodName.Dump       -> call handler encoded_args Handler.dump 
            MethodName.Shutdown   -> call handler encoded_args Handler.shutdown 


methodSize :: Proto.Int64
methodSize = Proto.messageSize $ Method MethodName.Ping


process :: (Handler h, ZMQ3.Receiver t, ZMQ3.Sender t) => ZMQ3.Socket z t -> h -> ZMQ3.ZMQ z ()
process socket handler = do
    ZMQ3.liftIO $ loggerIO debug "method processing started"
    encoded_request  <- ByteString.fromStrict <$> ZMQ3.receive socket
    encoded_response <- ByteString.toStrict   <$> selectCall handler encoded_request
    ZMQ3.send socket [] encoded_response
