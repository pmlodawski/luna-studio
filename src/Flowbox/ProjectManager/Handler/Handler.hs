---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.ProjectManager.Handler.Handler where

import           Flowbox.Bus.Data.Message                   (Message)
import           Flowbox.Bus.Data.Topic                     ((/+))
import qualified Flowbox.Bus.Data.Topic                     as Topic
import           Flowbox.Bus.RPC.HandlerMap                 (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                 as HandlerMap
import           Flowbox.Bus.RPC.RPC                        (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor           as Processor
import           Flowbox.Prelude                            hiding (error)
import           Flowbox.ProjectManager.Context             (ContextRef)
import qualified Flowbox.ProjectManager.Handler.AST         as ASTHandler
import qualified Flowbox.ProjectManager.Handler.Graph       as GraphHandler
import qualified Flowbox.ProjectManager.Handler.Library     as LibraryHandler
import qualified Flowbox.ProjectManager.Handler.NodeDefault as NodeDefaultHandler
import qualified Flowbox.ProjectManager.Handler.Project     as ProjectHandler
import qualified Flowbox.ProjectManager.Handler.Properties  as PropertiesHandler
import qualified Flowbox.ProjectManager.Topic               as Topic
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers               as Proto


logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.Handler"


handlerMap :: ContextRef -> HandlerMap IO
handlerMap ctx callback = HandlerMap.fromList
    [ (Topic.projectListRequest                                     , call Topic.status $ ProjectHandler.list ctx)
    , (Topic.projectLookupRequest                                   , call Topic.status $ ProjectHandler.lookup ctx)
    , (Topic.projectCreateRequest                                   , call Topic.update $ ProjectHandler.create ctx)
    , (Topic.projectOpenRequest                                     , call Topic.update $ ProjectHandler.open ctx)
    , (Topic.projectModifyRequest                                   , call Topic.update $ ProjectHandler.modify ctx)
    , (Topic.projectCloseRequest                                    , call Topic.update $ ProjectHandler.close ctx)
    , (Topic.projectStoreRequest                                    , call Topic.status $ ProjectHandler.store ctx)
    , (Topic.projectLibraryListRequest                              , call Topic.status $ LibraryHandler.list ctx)
    , (Topic.projectLibraryLookupRequest                            , call Topic.status $ LibraryHandler.lookup ctx)
    , (Topic.projectLibraryCreateRequest                            , call Topic.update $ LibraryHandler.create ctx)
    , (Topic.projectLibraryLoadRequest                              , call Topic.update $ LibraryHandler.load ctx)
    , (Topic.projectLibraryUnloadRequest                            , call Topic.update $ LibraryHandler.unload ctx)
    , (Topic.projectLibraryStoreRequest                             , call Topic.status $ LibraryHandler.store ctx)
    , (Topic.projectLibraryAstGetRequest                            , call Topic.status $ ASTHandler.get ctx)
    , (Topic.projectLibraryAstRemoveRequest                         , call Topic.update $ ASTHandler.remove ctx)
    , (Topic.projectLibraryAstResolveRequest                        , call Topic.status $ ASTHandler.resolve ctx)
    , (Topic.projectLibraryAstModuleAddRequest                      , call Topic.update $ ASTHandler.moduleAdd ctx)
    , (Topic.projectLibraryAstModuleModifyClsRequest                , call Topic.update $ ASTHandler.moduleClsModify ctx)
    , (Topic.projectLibraryAstModuleModifyFieldsRequest             , call Topic.update $ ASTHandler.moduleFieldsModify ctx)
    , (Topic.projectLibraryAstModuleModifyImportsRequest            , call Topic.update $ ASTHandler.moduleImportsModify ctx)
    , (Topic.projectLibraryAstDataAddRequest                        , call Topic.update $ ASTHandler.dataAdd ctx)
    , (Topic.projectLibraryAstDataModifyClassesRequest              , call Topic.update $ ASTHandler.dataClassesModify ctx)
    , (Topic.projectLibraryAstDataModifyClsRequest                  , call Topic.update $ ASTHandler.dataClsModify ctx)
    , (Topic.projectLibraryAstDataModifyConsRequest                 , call Topic.update $ ASTHandler.dataConsModify ctx)
    , (Topic.projectLibraryAstDataModifyMethodsRequest              , call Topic.update $ ASTHandler.dataMethodsModify ctx)
    , (Topic.projectLibraryAstFunctionAddRequest                    , call Topic.update $ ASTHandler.functionAdd ctx)
    , (Topic.projectLibraryAstFunctionModifyInputsRequest           , call Topic.update $ ASTHandler.functionInputsModify ctx)
    , (Topic.projectLibraryAstFunctionModifyNameRequest             , call Topic.update $ ASTHandler.functionNameModify ctx)
    , (Topic.projectLibraryAstFunctionModifyOutputRequest           , call Topic.update $ ASTHandler.functionOutputModify ctx)
    , (Topic.projectLibraryAstFunctionModifyPathRequest             , call Topic.update $ ASTHandler.functionPathModify ctx)
    , (Topic.projectLibraryAstFunctionGraphGetRequest               , call Topic.status $ GraphHandler.get ctx)
    , (Topic.projectLibraryAstFunctionGraphConnectRequest           , call Topic.update $ GraphHandler.connect ctx)
    , (Topic.projectLibraryAstFunctionGraphDisconnectRequest        , call Topic.update $ GraphHandler.disconnect ctx)
    , (Topic.projectLibraryAstFunctionGraphLookupRequest            , call Topic.status $ GraphHandler.lookup ctx)
    , (Topic.projectLibraryAstFunctionGraphNodeAddRequest           , call Topic.update $ GraphHandler.nodeAdd ctx)
    , (Topic.projectLibraryAstFunctionGraphNodeRemoveRequest        , call Topic.update $ GraphHandler.nodeRemove ctx)
    , (Topic.projectLibraryAstFunctionGraphNodeModifyRequest        , call Topic.update $ GraphHandler.nodeModify ctx)
    , (Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest , call Topic.update $ GraphHandler.nodeModifyInPlace ctx)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultGetRequest    , call Topic.status $ NodeDefaultHandler.get ctx)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultRemoveRequest , call Topic.update $ NodeDefaultHandler.remove ctx)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultSetRequest    , call Topic.update $ NodeDefaultHandler.set ctx)
    , (Topic.projectLibraryAstFunctionGraphNodePropertiesGetRequest , call Topic.status $ PropertiesHandler.getNodeProperties ctx)
    , (Topic.projectLibraryAstFunctionGraphNodePropertiesSetRequest , call Topic.update $ PropertiesHandler.setNodeProperties ctx)
    , (Topic.projectLibraryAstPropertiesGetRequest                  , call Topic.status $ PropertiesHandler.getASTProperties ctx)
    , (Topic.projectLibraryAstPropertiesSetRequest                  , call Topic.update $ PropertiesHandler.setASTProperties ctx)
    ]
    where
        call :: (Proto.Serializable args, Proto.Serializable result)
             => String -> (args -> RPC IO result) -> IO [Message]
        call type_ = callback ((/+) type_) . Processor.singleResult



