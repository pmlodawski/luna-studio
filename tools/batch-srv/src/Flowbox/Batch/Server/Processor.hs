
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Batch.Server.Processor where

import qualified Control.Concurrent               as Concurrent
import           Control.Concurrent.MVar          (MVar)
import qualified Control.Concurrent.MVar          as MVar
import           Data.ByteString.Lazy             (ByteString)
import qualified Data.Map                         as Map
import           Network.Socket                   (Socket)
import           Text.ProtocolBuffers             (Int32)
import qualified Text.ProtocolBuffers             as Proto
import qualified Text.ProtocolBuffers.Extensions  as Extensions
import qualified Text.ProtocolBuffers.Reflections as Reflections
import qualified Text.ProtocolBuffers.WireMessage as WireMessage

import           Flowbox.Batch.Server.Handler.Handler           (Handler)
import qualified Flowbox.Batch.Server.Handler.Handler           as Handler
import qualified Flowbox.Batch.Server.Transport.TCP.TCP         as TCP
import           Flowbox.Control.Error
import           Flowbox.Prelude                                hiding (error)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import           Generated.Proto.Batch.Exception                (Exception (Exception))
import qualified Generated.Proto.Batch.Exception                as Exception
import           Generated.Proto.Batch.Request                  (Request)
import qualified Generated.Proto.Batch.Request                  as Request
import qualified Generated.Proto.Batch.Request.Method           as Method
import           Generated.Proto.Batch.Response                 (Response (Response))
import qualified Generated.Proto.Batch.Response.Type            as ResponseType

import qualified Generated.Proto.Batch.AST.AddClass.Args                    as AddClass
import qualified Generated.Proto.Batch.AST.AddClass.Result                  as AddClass
import qualified Generated.Proto.Batch.AST.AddFunction.Args                 as AddFunction
import qualified Generated.Proto.Batch.AST.AddFunction.Result               as AddFunction
import qualified Generated.Proto.Batch.AST.AddModule.Args                   as AddModule
import qualified Generated.Proto.Batch.AST.AddModule.Result                 as AddModule
import qualified Generated.Proto.Batch.AST.Definitions.Args                 as Definitions
import qualified Generated.Proto.Batch.AST.Definitions.Result               as Definitions
import qualified Generated.Proto.Batch.AST.Remove.Args                      as Remove
import qualified Generated.Proto.Batch.AST.Remove.Result                    as Remove
import qualified Generated.Proto.Batch.AST.ResolveDefinition.Args           as ResolveDefinition
import qualified Generated.Proto.Batch.AST.ResolveDefinition.Result         as ResolveDefinition
import qualified Generated.Proto.Batch.AST.UpdateClassCls.Args              as UpdateClassCls
import qualified Generated.Proto.Batch.AST.UpdateClassCls.Result            as UpdateClassCls
import qualified Generated.Proto.Batch.AST.UpdateClassFields.Args           as UpdateClassFields
import qualified Generated.Proto.Batch.AST.UpdateClassFields.Result         as UpdateClassFields
import qualified Generated.Proto.Batch.AST.UpdateFunctionInputs.Args        as UpdateFunctionInputs
import qualified Generated.Proto.Batch.AST.UpdateFunctionInputs.Result      as UpdateFunctionInputs
import qualified Generated.Proto.Batch.AST.UpdateFunctionName.Args          as UpdateFunctionName
import qualified Generated.Proto.Batch.AST.UpdateFunctionName.Result        as UpdateFunctionName
import qualified Generated.Proto.Batch.AST.UpdateFunctionOutput.Args        as UpdateFunctionOutput
import qualified Generated.Proto.Batch.AST.UpdateFunctionOutput.Result      as UpdateFunctionOutput
import qualified Generated.Proto.Batch.AST.UpdateFunctionPath.Args          as UpdateFunctionPath
import qualified Generated.Proto.Batch.AST.UpdateFunctionPath.Result        as UpdateFunctionPath
import qualified Generated.Proto.Batch.AST.UpdateModuleCls.Args             as UpdateModuleCls
import qualified Generated.Proto.Batch.AST.UpdateModuleCls.Result           as UpdateModuleCls
import qualified Generated.Proto.Batch.AST.UpdateModuleFields.Args          as UpdateModuleFields
import qualified Generated.Proto.Batch.AST.UpdateModuleFields.Result        as UpdateModuleFields
import qualified Generated.Proto.Batch.AST.UpdateModuleImports.Args         as UpdateModuleImports
import qualified Generated.Proto.Batch.AST.UpdateModuleImports.Result       as UpdateModuleImports
import qualified Generated.Proto.Batch.FileSystem.CP.Args                   as CP
import qualified Generated.Proto.Batch.FileSystem.CP.Result                 as CP
import qualified Generated.Proto.Batch.FileSystem.LS.Args                   as LS
import qualified Generated.Proto.Batch.FileSystem.LS.Result                 as LS
import qualified Generated.Proto.Batch.FileSystem.MkDir.Args                as MkDir
import qualified Generated.Proto.Batch.FileSystem.MkDir.Result              as MkDir
import qualified Generated.Proto.Batch.FileSystem.MV.Args                   as MV
import qualified Generated.Proto.Batch.FileSystem.MV.Result                 as MV
import qualified Generated.Proto.Batch.FileSystem.RM.Args                   as RM
import qualified Generated.Proto.Batch.FileSystem.RM.Result                 as RM
import qualified Generated.Proto.Batch.FileSystem.Stat.Args                 as Stat
import qualified Generated.Proto.Batch.FileSystem.Stat.Result               as Stat
import qualified Generated.Proto.Batch.FileSystem.Touch.Args                as Touch
import qualified Generated.Proto.Batch.FileSystem.Touch.Result              as Touch
import qualified Generated.Proto.Batch.Graph.AddNode.Args                   as AddNode
import qualified Generated.Proto.Batch.Graph.AddNode.Result                 as AddNode
import qualified Generated.Proto.Batch.Graph.Connect.Args                   as Connect
import qualified Generated.Proto.Batch.Graph.Connect.Result                 as Connect
import qualified Generated.Proto.Batch.Graph.Disconnect.Args                as Disconnect
import qualified Generated.Proto.Batch.Graph.Disconnect.Result              as Disconnect
import qualified Generated.Proto.Batch.Graph.NodeByID.Args                  as NodeByID
import qualified Generated.Proto.Batch.Graph.NodeByID.Result                as NodeByID
import qualified Generated.Proto.Batch.Graph.NodesGraph.Args                as NodesGraph
import qualified Generated.Proto.Batch.Graph.NodesGraph.Result              as NodesGraph
import qualified Generated.Proto.Batch.Graph.RemoveNode.Args                as RemoveNode
import qualified Generated.Proto.Batch.Graph.RemoveNode.Result              as RemoveNode
import qualified Generated.Proto.Batch.Library.BuildLibrary.Args            as BuildLibrary
import qualified Generated.Proto.Batch.Library.BuildLibrary.Result          as BuildLibrary
import qualified Generated.Proto.Batch.Library.CreateLibrary.Args           as CreateLibrary
import qualified Generated.Proto.Batch.Library.CreateLibrary.Result         as CreateLibrary
import qualified Generated.Proto.Batch.Library.Libraries.Args               as Libraries
import qualified Generated.Proto.Batch.Library.Libraries.Result             as Libraries
import qualified Generated.Proto.Batch.Library.LibraryByID.Args             as LibraryByID
import qualified Generated.Proto.Batch.Library.LibraryByID.Result           as LibraryByID
import qualified Generated.Proto.Batch.Library.LoadLibrary.Args             as LoadLibrary
import qualified Generated.Proto.Batch.Library.LoadLibrary.Result           as LoadLibrary
import qualified Generated.Proto.Batch.Library.RunLibrary.Args              as RunLibrary
import qualified Generated.Proto.Batch.Library.RunLibrary.Result            as RunLibrary
import qualified Generated.Proto.Batch.Library.StoreLibrary.Args            as StoreLibrary
import qualified Generated.Proto.Batch.Library.StoreLibrary.Result          as StoreLibrary
import qualified Generated.Proto.Batch.Library.UnloadLibrary.Args           as UnloadLibrary
import qualified Generated.Proto.Batch.Library.UnloadLibrary.Result         as UnloadLibrary
import qualified Generated.Proto.Batch.Maintenance.Dump.Args                as Dump
import qualified Generated.Proto.Batch.Maintenance.Dump.Result              as Dump
import qualified Generated.Proto.Batch.Maintenance.Initialize.Args          as Initialize
import qualified Generated.Proto.Batch.Maintenance.Initialize.Result        as Initialize
import qualified Generated.Proto.Batch.Maintenance.Ping.Args                as Ping
import qualified Generated.Proto.Batch.Maintenance.Ping.Result              as Ping
import qualified Generated.Proto.Batch.Maintenance.Shutdown.Args            as Shutdown
import qualified Generated.Proto.Batch.Maintenance.Shutdown.Result          as Shutdown
import qualified Generated.Proto.Batch.NodeDefault.NodeDefaults.Args        as NodeDefaults
import qualified Generated.Proto.Batch.NodeDefault.NodeDefaults.Result      as NodeDefaults
import qualified Generated.Proto.Batch.NodeDefault.RemoveNodeDefault.Args   as RemoveNodeDefault
import qualified Generated.Proto.Batch.NodeDefault.RemoveNodeDefault.Result as RemoveNodeDefault
import qualified Generated.Proto.Batch.NodeDefault.SetNodeDefault.Args      as SetNodeDefault
import qualified Generated.Proto.Batch.NodeDefault.SetNodeDefault.Result    as SetNodeDefault
import qualified Generated.Proto.Batch.Parser.ParseExpr.Args                as ParseExpr
import qualified Generated.Proto.Batch.Parser.ParseExpr.Result              as ParseExpr
import qualified Generated.Proto.Batch.Parser.ParsePat.Args                 as ParsePat
import qualified Generated.Proto.Batch.Parser.ParsePat.Result               as ParsePat
import qualified Generated.Proto.Batch.Parser.ParseType.Args                as ParseType
import qualified Generated.Proto.Batch.Parser.ParseType.Result              as ParseType
import qualified Generated.Proto.Batch.Project.CloseProject.Args            as CloseProject
import qualified Generated.Proto.Batch.Project.CloseProject.Result          as CloseProject
import qualified Generated.Proto.Batch.Project.CreateProject.Args           as CreateProject
import qualified Generated.Proto.Batch.Project.CreateProject.Result         as CreateProject
import qualified Generated.Proto.Batch.Project.OpenProject.Args             as OpenProject
import qualified Generated.Proto.Batch.Project.OpenProject.Result           as OpenProject
import qualified Generated.Proto.Batch.Project.ProjectByID.Args             as ProjectByID
import qualified Generated.Proto.Batch.Project.ProjectByID.Result           as ProjectByID
import qualified Generated.Proto.Batch.Project.Projects.Args                as Projects
import qualified Generated.Proto.Batch.Project.Projects.Result              as Projects
import qualified Generated.Proto.Batch.Project.StoreProject.Args            as StoreProject
import qualified Generated.Proto.Batch.Project.StoreProject.Result          as StoreProject
import qualified Generated.Proto.Batch.Project.UpdateProject.Args           as UpdateProject
import qualified Generated.Proto.Batch.Project.UpdateProject.Result         as UpdateProject
import qualified Generated.Proto.Batch.Properties.GetProperties.Args        as GetProperties
import qualified Generated.Proto.Batch.Properties.GetProperties.Result      as GetProperties
import qualified Generated.Proto.Batch.Properties.SetProperties.Args        as SetProperties
import qualified Generated.Proto.Batch.Properties.SetProperties.Result      as SetProperties



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ.Processor"


responseExt :: ResponseType.Type -> Maybe Int32 -> r -> Extensions.Key Maybe Response r -> ByteString
responseExt t i r rspkey = Proto.messageWithLengthPut
                         $ Extensions.putExt rspkey (Just r)
                         $ Response t i $ Extensions.ExtField Map.empty

response :: ResponseType.Type -> Maybe Int32 -> ByteString
response t i = Proto.messageWithLengthPut
             $ Response t i $ Extensions.ExtField Map.empty


unsafeCall :: (WireMessage.Wire r, Reflections.ReflectDescriptor r, Show arg)
     => Request -> h ->  (h -> arg -> IO r)
     -> Extensions.Key Maybe Request arg
     -> Extensions.Key Maybe Response r
     -> IO ByteString
unsafeCall request handler method reqkey rspkey = do
    r <- case Extensions.getExt reqkey request of
        Right (Just args) -> do loggerIO debug $ show args
                                method handler args
        Left   e'         -> fail $ "Error while getting extension: " ++ e'
        _                 -> fail $ "Error while getting extension"
    return $ responseExt ResponseType.Result Nothing r rspkey


call :: (WireMessage.Wire r, Reflections.ReflectDescriptor r, Show arg)
     => Request -> h ->  (h -> arg -> IO r)
     -> Extensions.Key Maybe Request arg
     -> Extensions.Key Maybe Response r
     -> IO ByteString
call request handler method reqkey rspkey = do
    e <- runEitherT $ scriptIO $ unsafeCall request handler method reqkey rspkey
    case e of
        Left  m -> do loggerIO error m
                      let exc = Exception $ encodePJ m
                      return $ responseExt ResponseType.Exception Nothing exc Exception.rsp
        Right a ->    return a


async :: (WireMessage.Wire r, Reflections.ReflectDescriptor r, Show arg)
      => Request -> h ->  (h -> arg -> IO r)
      -> Extensions.Key Maybe Request arg
      -> Extensions.Key Maybe Response r
      -> MVar Socket
      -> IO ByteString
async request handler method reqkey rspkey notifySocket = do
    _ <- Concurrent.forkIO $ do b <- call request handler method reqkey rspkey
                                MVar.withMVar notifySocket (\s -> TCP.sendData s b)
    return $ response ResponseType.Accept (Just 0)


process :: Handler h => MVar Socket -> h -> ByteString -> IO ByteString
process notifySocket handler encoded_request = case Proto.messageWithLengthGet encoded_request of
                                     -- TODO [PM] : move messageWithLengthGet from here
    Left   e           -> fail $ "Error while decoding request: " ++ e
    Right (request, _) -> case Request.method request of
        Method.AST_AddModule            -> call request handler Handler.addModule            AddModule.req            AddModule.rsp
        Method.AST_AddClass             -> call request handler Handler.addClass             AddClass.req             AddClass.rsp
        Method.AST_AddFunction          -> call request handler Handler.addFunction          AddFunction.req          AddFunction.rsp
        Method.AST_Definitions          -> call request handler Handler.definitions          Definitions.req          Definitions.rsp
        Method.AST_UpdateModuleCls      -> call request handler Handler.updateModuleCls      UpdateModuleCls.req      UpdateModuleCls.rsp
        Method.AST_UpdateModuleImports  -> call request handler Handler.updateModuleImports  UpdateModuleImports.req  UpdateModuleImports.rsp
        Method.AST_UpdateModuleFields   -> call request handler Handler.updateModuleFields   UpdateModuleFields.req   UpdateModuleFields.rsp
        Method.AST_UpdateClassCls       -> call request handler Handler.updateClassCls       UpdateClassCls.req       UpdateClassCls.rsp
        Method.AST_UpdateClassFields    -> call request handler Handler.updateClassFields    UpdateClassFields.req    UpdateClassFields.rsp
        Method.AST_UpdateFunctionName   -> call request handler Handler.updateFunctionName   UpdateFunctionName.req   UpdateFunctionName.rsp
        Method.AST_UpdateFunctionPath   -> call request handler Handler.updateFunctionPath   UpdateFunctionPath.req   UpdateFunctionPath.rsp
        Method.AST_UpdateFunctionInputs -> call request handler Handler.updateFunctionInputs UpdateFunctionInputs.req UpdateFunctionInputs.rsp
        Method.AST_UpdateFunctionOutput -> call request handler Handler.updateFunctionOutput UpdateFunctionOutput.req UpdateFunctionOutput.rsp
        Method.AST_Remove               -> call request handler Handler.remove               Remove.req               Remove.rsp
        Method.AST_ResolveDefinition    -> call request handler Handler.resolveDefinition    ResolveDefinition.req    ResolveDefinition.rsp

        Method.FileSystem_LS    -> call request handler Handler.ls    LS.req    LS.rsp
        Method.FileSystem_Stat  -> call request handler Handler.stat  Stat.req  Stat.rsp
        Method.FileSystem_MkDir -> call request handler Handler.mkdir MkDir.req MkDir.rsp
        Method.FileSystem_Touch -> call request handler Handler.touch Touch.req Touch.rsp
        Method.FileSystem_RM    -> call request handler Handler.rm    RM.req    RM.rsp
        Method.FileSystem_CP    -> call request handler Handler.cp    CP.req    CP.rsp
        Method.FileSystem_MV    -> call request handler Handler.mv    MV.req    MV.rsp

        Method.Graph_NodesGraph -> call request handler Handler.nodesGraph NodesGraph.req NodesGraph.rsp
        Method.Graph_NodeByID   -> call request handler Handler.nodeByID   NodeByID.req   NodeByID.rsp
        Method.Graph_AddNode    -> call request handler Handler.addNode    AddNode.req    AddNode.rsp
        Method.Graph_RemoveNode -> call request handler Handler.removeNode RemoveNode.req RemoveNode.rsp
        Method.Graph_Connect    -> call request handler Handler.connect    Connect.req    Connect.rsp
        Method.Graph_Disconnect -> call request handler Handler.disconnect Disconnect.req Disconnect.rsp

        Method.Library_Libraries     -> call request handler Handler.libraries     Libraries.req     Libraries.rsp
        Method.Library_LibraryByID   -> call request handler Handler.libraryByID   LibraryByID.req   LibraryByID.rsp
        Method.Library_CreateLibrary -> call request handler Handler.createLibrary CreateLibrary.req CreateLibrary.rsp
        Method.Library_LoadLibrary   -> call request handler Handler.loadLibrary   LoadLibrary.req   LoadLibrary.rsp
        Method.Library_UnloadLibrary -> call request handler Handler.unloadLibrary UnloadLibrary.req UnloadLibrary.rsp
        Method.Library_StoreLibrary  -> call request handler Handler.storeLibrary  StoreLibrary.req  StoreLibrary.rsp
        Method.Library_BuildLibrary  -> async request handler Handler.buildLibrary  BuildLibrary.req  BuildLibrary.rsp notifySocket
        Method.Library_RunLibrary    -> call request handler Handler.runLibrary    RunLibrary.req    RunLibrary.rsp

        Method.Maintenance_Initialize -> call request handler Handler.initialize Initialize.req Initialize.rsp
        Method.Maintenance_Ping       -> call request handler Handler.ping       Ping.req       Ping.rsp
        Method.Maintenance_Dump       -> call request handler Handler.dump       Dump.req       Dump.rsp
        Method.Maintenance_Shutdown   -> call request handler Handler.shutdown   Shutdown.req   Shutdown.rsp

        Method.NodeDefault_NodeDefaults      -> call request handler Handler.nodeDefaults      NodeDefaults.req      NodeDefaults.rsp
        Method.NodeDefault_SetNodeDefault    -> call request handler Handler.setNodeDefault    SetNodeDefault.req    SetNodeDefault.rsp
        Method.NodeDefault_RemoveNodeDefault -> call request handler Handler.removeNodeDefault RemoveNodeDefault.req RemoveNodeDefault.rsp

        Method.Parser_ParseExpr -> call request handler Handler.parseExpr ParseExpr.req ParseExpr.rsp
        Method.Parser_ParsePat  -> call request handler Handler.parsePat  ParsePat.req  ParsePat.rsp
        Method.Parser_ParseType -> call request handler Handler.parseType ParseType.req ParseType.rsp

        Method.Project_Projects      -> call request handler Handler.projects      Projects.req      Projects.rsp
        Method.Project_ProjectByID   -> call request handler Handler.projectByID   ProjectByID.req   ProjectByID.rsp
        Method.Project_CreateProject -> call request handler Handler.createProject CreateProject.req CreateProject.rsp
        Method.Project_OpenProject   -> call request handler Handler.openProject   OpenProject.req   OpenProject.rsp
        Method.Project_UpdateProject -> call request handler Handler.updateProject UpdateProject.req UpdateProject.rsp
        Method.Project_CloseProject  -> call request handler Handler.closeProject  CloseProject.req  CloseProject.rsp
        Method.Project_StoreProject  -> call request handler Handler.storeProject  StoreProject.req  StoreProject.rsp

        Method.Properties_GetProperties -> call request handler Handler.getProperties GetProperties.req GetProperties.rsp
        Method.Properties_SetProperties -> call request handler Handler.setProperties SetProperties.req SetProperties.rsp

