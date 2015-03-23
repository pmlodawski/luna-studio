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

import Control.Monad.Trans.State

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
logger = getLoggerIO $(moduleName)


handlerMap :: HandlerMap Context IO
handlerMap callback = HandlerMap.fromList
    [ (Topic.projectListRequest                                     , call Topic.status ProjectHandler.list)
    , (Topic.projectLookupRequest                                   , call Topic.status ProjectHandler.lookup)
    , (Topic.projectCreateRequest                                   , call Topic.update ProjectHandler.create)
    , (Topic.projectOpenRequest                                     , call Topic.update ProjectHandler.open)
    , (Topic.projectModifyRequest                                   , call Topic.update ProjectHandler.modify)
    , (Topic.projectCloseRequest                                    , call Topic.update ProjectHandler.close)
    , (Topic.projectStoreRequest                                    , call Topic.status ProjectHandler.store)
    , (Topic.projectLibraryListRequest                              , call Topic.status LibraryHandler.list)
    , (Topic.projectLibraryLookupRequest                            , call Topic.status LibraryHandler.lookup)
    , (Topic.projectLibraryCreateRequest                            , call Topic.update LibraryHandler.create)
    , (Topic.projectLibraryModifyRequest                            , call Topic.update LibraryHandler.modify)
    , (Topic.projectLibraryLoadRequest                              , call Topic.update LibraryHandler.load)
    , (Topic.projectLibraryUnloadRequest                            , call Topic.update LibraryHandler.unload)
    , (Topic.projectLibraryStoreRequest                             , call Topic.status LibraryHandler.store)
    , (Topic.projectLibraryAstGetRequest                            , call Topic.status ASTHandler.get)
    , (Topic.projectLibraryAstRemoveRequest                         , cleanCall (/+ Topic.update) ASTHandler.remove $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstRemoveRequest                       , cleanCall (/* Topic.update) ASTHandler.remove Nothing)
    , (Topic.projectLibraryAstResolveRequest                        , call Topic.status ASTHandler.resolve)
    , (Topic.projectLibraryAstModuleAddRequest                      , cleanCall (/+ Topic.update) ASTHandler.addModule $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstModuleAddRequest                    , cleanCall (/* Topic.update) ASTHandler.addModule Nothing)
    , (Topic.projectLibraryAstModuleModifyClsRequest                , call Topic.update ASTHandler.modifyModuleCls)
    , (Topic.projectLibraryAstModuleModifyFieldsRequest             , call Topic.update ASTHandler.modifyModuleFields)
    , (Topic.projectLibraryAstModuleModifyTypeAliasesRequest        , call Topic.update ASTHandler.modifyModuleTypeAliases)
    , (Topic.projectLibraryAstModuleModifyTypeDefsRequest           , call Topic.update ASTHandler.modifyModuleTypeDefs)
    , (Topic.projectLibraryAstModuleModifyImportsRequest            , call Topic.update ASTHandler.modifyModuleImports)
    , (Topic.projectLibraryAstDataAddRequest                        , call Topic.update ASTHandler.addData)
    , (Topic.projectLibraryAstDataModifyClassesRequest              , call Topic.update ASTHandler.modifyDataClasses)
    , (Topic.projectLibraryAstDataModifyClsRequest                  , call Topic.update ASTHandler.modifyDataCls)
    , (Topic.projectLibraryAstDataModifyConsRequest                 , call Topic.update ASTHandler.modifyDataCons)
    , (Topic.projectLibraryAstDataConModifyRequest                  , call Topic.update ASTHandler.modifyDataCon)
    , (Topic.projectLibraryAstDataConAddRequest                     , call Topic.update ASTHandler.addDataCon)
    , (Topic.projectLibraryAstDataConDeleteRequest                  , call Topic.update ASTHandler.deleteDataCon)
    , (Topic.projectLibraryAstDataConFieldAddRequest                , call Topic.update ASTHandler.addDataConField)
    , (Topic.projectLibraryAstDataConFieldDeleteRequest             , call Topic.update ASTHandler.deleteDataConField)
    , (Topic.projectLibraryAstDataConFieldModifyRequest             , call Topic.update ASTHandler.modifyDataConField)
    , (Topic.projectLibraryAstDataModifyMethodsRequest              , call Topic.update ASTHandler.modifyDataMethods)
    , (Topic.projectLibraryAstFunctionAddRequest                    , call Topic.update ASTHandler.addFunction)
    , (Topic.projectLibraryAstFunctionModifyInputsRequest           , call Topic.update ASTHandler.modifyFunctionInputs)
    , (Topic.projectLibraryAstFunctionModifyNameRequest             , call Topic.update ASTHandler.modifyFunctionName)
    , (Topic.projectLibraryAstFunctionModifyOutputRequest           , call Topic.update ASTHandler.modifyFunctionOutput)
    , (Topic.projectLibraryAstFunctionModifyPathRequest             , call Topic.update ASTHandler.modifyFunctionPath)
    , (Topic.projectLibraryAstFunctionGraphGetRequest               , call Topic.status GraphHandler.get)
    , (Topic.projectLibraryAstFunctionGraphConnectRequest           , cleanCall (/+ Topic.update) GraphHandler.connect $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphConnectRequest         , cleanCall (/* Topic.update) GraphHandler.connect Nothing)
    , (Topic.projectLibraryAstFunctionGraphDisconnectRequest        , cleanCall (/+ Topic.update) GraphHandler.disconnect $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphDisconnectRequest      , cleanCall (/* Topic.update) GraphHandler.disconnect Nothing)
    , (Topic.projectLibraryAstFunctionGraphLookupRequest            , call Topic.status GraphHandler.lookup)
    , (Topic.projectLibraryAstFunctionGraphLookupManyRequest        , call Topic.status GraphHandler.lookupMany)
    , (Topic.projectLibraryAstFunctionGraphNodeAddRequest           , cleanCall (/+ Topic.update) GraphHandler.nodeAdd $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphNodeAddRequest         , cleanCall (/* Topic.update) GraphHandler.nodeAdd Nothing)
    , (Topic.projectLibraryAstFunctionGraphNodeRemoveRequest        , cleanCall (/+ Topic.update) GraphHandler.nodeRemove $ Just Topic.urmRegisterMultipleRequest)
    , (u Topic.projectLibraryAstFunctionGraphNodeRemoveRequest      , cleanCall (/* Topic.update) GraphHandler.nodeRemove Nothing)
    , (Topic.projectLibraryAstFunctionGraphNodeModifyRequest        , cleanCall (/+ Topic.update) GraphHandler.nodeModify $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphNodeModifyRequest      , cleanCall (/* Topic.update) GraphHandler.nodeModify Nothing)
    , (Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest , cleanCall (/+ Topic.update) GraphHandler.nodeModifyInPlace $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest,cleanCall (/* Topic.update) GraphHandler.nodeModifyInPlace Nothing)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultGetRequest    , call Topic.status NodeDefaultHandler.get)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultRemoveRequest , cleanCall (/+ Topic.update) NodeDefaultHandler.remove $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphNodeDefaultRemoveRequest,cleanCall (/* Topic.update) NodeDefaultHandler.remove Nothing)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultSetRequest    , cleanCall (/+ Topic.update) NodeDefaultHandler.set $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphNodeDefaultSetRequest  , cleanCall (/* Topic.update) NodeDefaultHandler.set Nothing)
    , (Topic.projectLibraryAstFunctionGraphNodePropertiesGetRequest , call Topic.status PropertiesHandler.getNodeProperties)
    , (Topic.projectLibraryAstFunctionGraphNodePropertiesSetRequest , cleanCall (/+ Topic.update) PropertiesHandler.setNodeProperties $ Just Topic.urmRegisterRequest)
    , (u Topic.projectLibraryAstFunctionGraphNodePropertiesSetRequest,cleanCall (/* Topic.update) PropertiesHandler.setNodeProperties Nothing)
    , (Topic.projectLibraryAstPropertiesGetRequest                  , call Topic.status PropertiesHandler.getASTProperties)
    , (Topic.projectLibraryAstPropertiesSetRequest                  , call Topic.update PropertiesHandler.setASTProperties)
    , (Topic.projectLibraryAstCodeGetRequest                        , call Topic.status ASTHandler.getCode)
    , (Topic.projectLibraryAstCodeSetRequest                        , call Topic.update ASTHandler.setCode)
    , (Topic.projectmanagerSyncGetRequest                           , call Topic.status SyncHandler.syncGet)
    , (Topic.projectmanagerPingRequest                              , call Topic.status MaintenanceHandler.ping)
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
