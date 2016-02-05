---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Flowbox.ProjectManager.RPC.Handler.Handler where

import           Control.Monad.Trans.State

import           Flowbox.Bus.Data.Message                       (Message)
import           Flowbox.Bus.Data.Topic                         ((/+))
import           Flowbox.Bus.Data.Topic                         (Topic)
import qualified Flowbox.Bus.Data.Topic                         as Topic
import           Flowbox.Bus.RPC.HandlerMap                     (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                     as HandlerMap
import           Flowbox.Bus.RPC.RPC                            (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor               as Processor
import           Flowbox.Prelude                                hiding (Context, error)
import           Flowbox.ProjectManager.Context                 (Context)
import qualified Flowbox.ProjectManager.RPC.Handler.AST         as ASTHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Graph       as GraphHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Library     as LibraryHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Maintenance as MaintenanceHandler
import qualified Flowbox.ProjectManager.RPC.Handler.NodeDefault as NodeDefaultHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Project     as ProjectHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Properties  as PropertiesHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Sync        as SyncHandler
import qualified Flowbox.ProjectManager.RPC.Topic               as Topic
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                   as Proto
import qualified Flowbox.UR.Manager.RPC.Topic                   as Topic



logger :: LoggerIO
logger = getLoggerIO $moduleName


handlerMap :: HandlerMap Context IO
handlerMap callback = HandlerMap.fromList
    [ (  Topic.projectCloseRequest                                   , cleanCall (/+ Topic.update) ProjectHandler.close $ Just Topic.urmClearStackRequest)
    , (u Topic.projectCloseRequest                                   , cleanCall (/* Topic.update) ProjectHandler.close $ Just Topic.urmClearStackRequest)
    , (  Topic.projectCreateRequest                                  , call Topic.update ProjectHandler.create)
    , (  Topic.projectLibraryAstCodeGetRequest                       , call Topic.status ASTHandler.getCode)
    , (  Topic.projectLibraryAstCodeSetRequest                       , cleanCall (/+ Topic.update) ASTHandler.setCode $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstCodeSetRequest                       , cleanCall (/* Topic.update) ASTHandler.setCode $ Nothing)
    , (  Topic.projectLibraryAstDataAddRequest                       , cleanCall (/+ Topic.update) ASTHandler.addData $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstDataAddRequest                       , cleanCall (/* Topic.update) ASTHandler.addData Nothing)
    , (  Topic.projectLibraryAstDataConAddRequest                    , call Topic.update ASTHandler.addDataCon)
    , (  Topic.projectLibraryAstDataConDeleteRequest                 , call Topic.update ASTHandler.deleteDataCon)
    , (  Topic.projectLibraryAstDataConFieldAddRequest               , call Topic.update ASTHandler.addDataConField)
    , (  Topic.projectLibraryAstDataConFieldDeleteRequest            , call Topic.update ASTHandler.deleteDataConField)
    , (  Topic.projectLibraryAstDataConFieldModifyRequest            , call Topic.update ASTHandler.modifyDataConField)
    , (  Topic.projectLibraryAstDataConModifyRequest                 , call Topic.update ASTHandler.modifyDataCon)
    , (  Topic.projectLibraryAstDataModifyClassesRequest             , call Topic.update ASTHandler.modifyDataClasses)
    , (  Topic.projectLibraryAstDataModifyClsRequest                 , cleanCall (/+ Topic.update) ASTHandler.modifyDataCls $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstDataModifyClsRequest                 , cleanCall (/* Topic.update) ASTHandler.modifyDataCls Nothing)
    , (  Topic.projectLibraryAstDataModifyConsRequest                , call Topic.update ASTHandler.modifyDataCons)
    , (  Topic.projectLibraryAstDataModifyMethodsRequest             , call Topic.update ASTHandler.modifyDataMethods)
    , (  Topic.projectLibraryAstFunctionAddRequest                   , cleanCall (/+ Topic.update) ASTHandler.addFunction $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionAddRequest                   , cleanCall (/* Topic.update) ASTHandler.addFunction Nothing)
    , (  Topic.projectLibraryAstFunctionGraphConnectRequest          , cleanCall (/+ Topic.update) GraphHandler.connect $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphConnectRequest          , cleanCall (/* Topic.update) GraphHandler.connect Nothing)
    , (  Topic.projectLibraryAstFunctionGraphDisconnectRequest       , cleanCall (/+ Topic.update) GraphHandler.disconnect $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphDisconnectRequest       , cleanCall (/* Topic.update) GraphHandler.disconnect Nothing)
    , (  Topic.projectLibraryAstFunctionGraphGetRequest              , call Topic.status GraphHandler.get)
    , (  Topic.projectLibraryAstFunctionGraphLookupManyRequest       , call Topic.status GraphHandler.lookupMany)
    , (  Topic.projectLibraryAstFunctionGraphLookupRequest           , call Topic.status GraphHandler.lookup)
    , (  Topic.projectLibraryAstFunctionGraphNodeAddRequest          , cleanCall (/+ Topic.update) GraphHandler.nodeAdd $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphNodeAddRequest          , cleanCall (/* Topic.update) GraphHandler.nodeAdd Nothing)
    , (  Topic.projectLibraryAstFunctionGraphNodeDefaultGetRequest   , call Topic.status NodeDefaultHandler.get)
    , (  Topic.projectLibraryAstFunctionGraphNodeDefaultRemoveRequest, call Topic.update NodeDefaultHandler.remove)
    , (  Topic.projectLibraryAstFunctionGraphNodeDefaultSetRequest   , cleanCall (/+ Topic.update) NodeDefaultHandler.set $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphNodeDefaultSetRequest   , cleanCall (/* Topic.update) NodeDefaultHandler.set Nothing)
    , (  Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest, cleanCall (/+ Topic.update) GraphHandler.nodeModifyInPlace $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest, cleanCall (/* Topic.update) GraphHandler.nodeModifyInPlace Nothing)
    , (  Topic.projectLibraryAstFunctionGraphNodeModifyRequest       , cleanCall (/+ Topic.update) GraphHandler.nodeModify $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphNodeModifyRequest       , cleanCall (/* Topic.update) GraphHandler.nodeModify Nothing)
    , (  Topic.projectLibraryAstFunctionGraphNodePropertiesGetRequest, call Topic.status PropertiesHandler.getNodeProperties)
    , (  Topic.projectLibraryAstFunctionGraphNodePropertiesSetRequest, cleanCall (/+ Topic.update) PropertiesHandler.setNodeProperties $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphNodePropertiesSetRequest, cleanCall (/* Topic.update) PropertiesHandler.setNodeProperties Nothing)
    , (  Topic.projectLibraryAstFunctionGraphNodeRemoveRequest       , cleanCall (/+ Topic.update) GraphHandler.nodeRemove $ Just Topic.urmRegisterMultipleRequest)
    , (u Topic.projectLibraryAstFunctionGraphNodeRemoveRequest       , cleanCall (/* Topic.update) GraphHandler.nodeRemove Nothing)
    , (  Topic.projectLibraryAstFunctionModifyInputsRequest          , cleanCall (/+ Topic.update) ASTHandler.modifyFunctionInputs $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionModifyInputsRequest          , cleanCall (/* Topic.update) ASTHandler.modifyFunctionInputs Nothing)
    , (  Topic.projectLibraryAstFunctionModifyNameRequest            , cleanCall (/+ Topic.update) ASTHandler.modifyFunctionName $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionModifyNameRequest            , cleanCall (/* Topic.update) ASTHandler.modifyFunctionName Nothing)
    , (  Topic.projectLibraryAstFunctionModifyOutputRequest          , cleanCall (/+ Topic.update) ASTHandler.modifyFunctionOutput $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionModifyOutputRequest          , cleanCall (/* Topic.update) ASTHandler.modifyFunctionOutput Nothing)
    , (  Topic.projectLibraryAstFunctionModifyPathRequest            , call Topic.update ASTHandler.modifyFunctionPath)
    , (  Topic.projectLibraryAstGetRequest                           , call Topic.status ASTHandler.get)
    , (  Topic.projectLibraryAstModuleAddRequest                     , cleanCall (/+ Topic.update) ASTHandler.addModule $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstModuleAddRequest                     , cleanCall (/* Topic.update) ASTHandler.addModule Nothing)
    , (  Topic.projectLibraryAstModuleModifyClsRequest               , call Topic.update ASTHandler.modifyModuleCls)
    , (  Topic.projectLibraryAstModuleModifyFieldsRequest            , call Topic.update ASTHandler.modifyModuleFields)
    , (  Topic.projectLibraryAstModuleModifyImportsRequest           , call Topic.update ASTHandler.modifyModuleImports)
    , (  Topic.projectLibraryAstModuleModifyTypeAliasesRequest       , call Topic.update ASTHandler.modifyModuleTypeAliases)
    , (  Topic.projectLibraryAstModuleModifyTypeDefsRequest          , call Topic.update ASTHandler.modifyModuleTypeDefs)
    , (  Topic.projectLibraryAstPropertiesGetRequest                 , call Topic.status PropertiesHandler.getASTProperties)
    , (  Topic.projectLibraryAstPropertiesSetRequest                 , cleanCall (/+ Topic.update) PropertiesHandler.setASTProperties $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstPropertiesSetRequest                 , cleanCall (/* Topic.update) PropertiesHandler.setASTProperties Nothing)
    , (  Topic.projectLibraryAstRemoveRequest                        , cleanCall (/+ Topic.update) ASTHandler.remove $ Just Topic.urmRegisterMultipleRequest)
    , (u Topic.projectLibraryAstRemoveRequest                        , cleanCall (/* Topic.update) ASTHandler.remove Nothing)
    , (  Topic.projectLibraryAstResolveRequest                       , call Topic.status ASTHandler.resolve)
    , (  Topic.projectLibraryCreateRequest                           , call Topic.update LibraryHandler.create)
    , (  Topic.projectLibraryListRequest                             , call Topic.status LibraryHandler.list)
    , (  Topic.projectLibraryLoadRequest                             , call Topic.update LibraryHandler.load)
    , (  Topic.projectLibraryLookupRequest                           , call Topic.status LibraryHandler.lookup)
    , (  Topic.projectLibraryModifyRequest                           , call Topic.update LibraryHandler.modify)
    , (  Topic.projectLibraryStoreRequest                            , call Topic.status LibraryHandler.store)
    , (  Topic.projectLibraryUnloadRequest                           , call Topic.update LibraryHandler.unload)
    , (  Topic.projectLibraryUpdateRequest                           , call Topic.update LibraryHandler.modify)
    , (  Topic.projectListRequest                                    , call Topic.status ProjectHandler.list)
    , (  Topic.projectLookupRequest                                  , call Topic.status ProjectHandler.lookup)
    , (  Topic.projectmanagerPingRequest                             , call Topic.status MaintenanceHandler.ping)
    , (  Topic.projectmanagerSyncGetRequest                          , call Topic.status SyncHandler.syncGet)
    , (  Topic.projectModifyRequest                                  , call Topic.update ProjectHandler.modify)
    , (  Topic.projectOpenRequest                                    , call Topic.update ProjectHandler.open)
    , (  Topic.projectStoreRequest                                   , call Topic.status ProjectHandler.store)
    ]
    where
        call :: (Proto.Serializable args, Proto.Serializable result)
             => String -> (args -> RPC Context IO result) -> StateT Context IO [Message]
        call type_ = callback (/+ type_) . Processor.singleResult
        cleanCall :: (Proto.Serializable args, Proto.Serializable result)
                  => (Topic -> Topic) -> (args -> Maybe Topic -> RPC Context IO ([result], [Message])) -> Maybe Topic -> StateT Context IO [Message]
        cleanCall topiConstr fun = callback topiConstr . flip fun


        (/*) :: Topic -> Topic -> Topic
        a /* b = tail (dropWhile (/= '.') a) /+ b
--        (/*) = (/+) . tail . dropWhile ('.' /=) -- co lepsze?


        u :: Topic -> Topic
        u = ("undone." ++)
