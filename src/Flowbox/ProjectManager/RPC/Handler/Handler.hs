---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.ProjectManager.RPC.Handler.Handler where

import Control.Monad.Trans.State

import           Flowbox.Bus.Data.Message                       (Message)
import           Flowbox.Bus.Data.Topic                         ((/+))
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



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.RPC.Handler.Handler"


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
    , (Topic.projectLibraryLoadRequest                              , call Topic.update LibraryHandler.load)
    , (Topic.projectLibraryUnloadRequest                            , call Topic.update LibraryHandler.unload)
    , (Topic.projectLibraryStoreRequest                             , call Topic.status LibraryHandler.store)
    , (Topic.projectLibraryAstGetRequest                            , call Topic.status ASTHandler.get)
    , (Topic.projectLibraryAstRemoveRequest                         , call Topic.update ASTHandler.remove)
    , (Topic.projectLibraryAstResolveRequest                        , call Topic.status ASTHandler.resolve)
    , (Topic.projectLibraryAstModuleAddRequest                      , call Topic.update ASTHandler.moduleAdd)
    , (Topic.projectLibraryAstModuleModifyClsRequest                , call Topic.update ASTHandler.moduleClsModify)
    , (Topic.projectLibraryAstModuleModifyFieldsRequest             , call Topic.update ASTHandler.moduleFieldsModify)
    , (Topic.projectLibraryAstModuleModifyImportsRequest            , call Topic.update ASTHandler.moduleImportsModify)
    , (Topic.projectLibraryAstDataAddRequest                        , call Topic.update ASTHandler.dataAdd)
    , (Topic.projectLibraryAstDataModifyClassesRequest              , call Topic.update ASTHandler.dataClassesModify)
    , (Topic.projectLibraryAstDataModifyClsRequest                  , call Topic.update ASTHandler.dataClsModify)
    , (Topic.projectLibraryAstDataModifyConsRequest                 , call Topic.update ASTHandler.dataConsModify)
    , (Topic.projectLibraryAstDataModifyMethodsRequest              , call Topic.update ASTHandler.dataMethodsModify)
    , (Topic.projectLibraryAstFunctionAddRequest                    , call Topic.update ASTHandler.functionAdd)
    , (Topic.projectLibraryAstFunctionModifyInputsRequest           , call Topic.update ASTHandler.functionInputsModify)
    , (Topic.projectLibraryAstFunctionModifyNameRequest             , call Topic.update ASTHandler.functionNameModify)
    , (Topic.projectLibraryAstFunctionModifyOutputRequest           , call Topic.update ASTHandler.functionOutputModify)
    , (Topic.projectLibraryAstFunctionModifyPathRequest             , call Topic.update ASTHandler.functionPathModify)
    , (Topic.projectLibraryAstFunctionGraphGetRequest               , call Topic.status GraphHandler.get)
    , (Topic.projectLibraryAstFunctionGraphConnectRequest           , call Topic.update GraphHandler.connect)
    , (Topic.projectLibraryAstFunctionGraphDisconnectRequest        , call Topic.update GraphHandler.disconnect)
    , (Topic.projectLibraryAstFunctionGraphLookupRequest            , call Topic.status GraphHandler.lookup)
    , (Topic.projectLibraryAstFunctionGraphNodeAddRequest           , call Topic.update GraphHandler.nodeAdd)
    , (Topic.projectLibraryAstFunctionGraphNodeRemoveRequest        , call Topic.update GraphHandler.nodeRemove)
    , (Topic.projectLibraryAstFunctionGraphNodeModifyRequest        , call Topic.update GraphHandler.nodeModify)
    , (Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest , call Topic.update GraphHandler.nodeModifyInPlace)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultGetRequest    , call Topic.status NodeDefaultHandler.get)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultRemoveRequest , call Topic.update NodeDefaultHandler.remove)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultSetRequest    , call Topic.update NodeDefaultHandler.set)
    , (Topic.projectLibraryAstFunctionGraphNodePropertiesGetRequest , call Topic.status PropertiesHandler.getNodeProperties)
    , (Topic.projectLibraryAstFunctionGraphNodePropertiesSetRequest , call Topic.update PropertiesHandler.setNodeProperties)
    , (Topic.projectLibraryAstPropertiesGetRequest                  , call Topic.status PropertiesHandler.getASTProperties)
    , (Topic.projectLibraryAstPropertiesSetRequest                  , call Topic.update PropertiesHandler.setASTProperties)
    , (Topic.projectmanagerSyncGetRequest                           , call Topic.status SyncHandler.syncGet)
    , (Topic.projectmanagerPingRequest                              , call Topic.status MaintenanceHandler.ping)
    ]
    where
        call :: (Proto.Serializable args, Proto.Serializable result)
             => String -> (args -> RPC Context IO result) -> StateT Context IO [Message]
        call type_ = callback (/+ type_) . Processor.singleResult
