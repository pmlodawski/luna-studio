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
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers               as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.Handler"


handlerMap :: ContextRef -> HandlerMap IO
handlerMap ctx callback = HandlerMap.fromList
    [ ("project.list.request"          , call Topic.status $ ProjectHandler.list ctx)
    , ("project.lookup.request"        , call Topic.status $ ProjectHandler.lookup ctx)
    , ("project.create.request"        , call Topic.update $ ProjectHandler.create ctx)
    , ("project.open.request"          , call Topic.update $ ProjectHandler.open ctx)
    , ("project.modify.request"        , call Topic.update $ ProjectHandler.modify ctx)
    , ("project.close.request"         , call Topic.update $ ProjectHandler.close ctx)
    , ("project.store.request"         , call Topic.status $ ProjectHandler.store ctx)
    , ("project.library.list.request"  , call Topic.status $ LibraryHandler.list ctx)
    , ("project.library.lookup.request", call Topic.status $ LibraryHandler.lookup ctx)
    , ("project.library.create.request", call Topic.update $ LibraryHandler.create ctx)
    , ("project.library.load.request"  , call Topic.update $ LibraryHandler.load ctx)
    , ("project.library.unload.request", call Topic.update $ LibraryHandler.unload ctx)
    , ("project.library.store.request" , call Topic.status $ LibraryHandler.store ctx)
    , ("project.library.ast.get.request"                   , call Topic.status $ ASTHandler.get ctx)
    , ("project.library.ast.remove.request"                , call Topic.update $ ASTHandler.remove ctx)
    , ("project.library.ast.resolve.request"               , call Topic.status $ ASTHandler.resolve ctx)
    , ("project.library.ast.module.add.request"            , call Topic.update $ ASTHandler.moduleAdd ctx)
    , ("project.library.ast.module.modify.cls.request"     , call Topic.update $ ASTHandler.moduleClsModify ctx)
    , ("project.library.ast.module.modify.fields.request"  , call Topic.update $ ASTHandler.moduleFieldsModify ctx)
    , ("project.library.ast.module.modify.imports.request" , call Topic.update $ ASTHandler.moduleImportsModify ctx)
    , ("project.library.ast.data.add.request"              , call Topic.update $ ASTHandler.dataAdd ctx)
    , ("project.library.ast.data.modify.classes.request"   , call Topic.update $ ASTHandler.dataClassesModify ctx)
    , ("project.library.ast.data.modify.cls.request"       , call Topic.update $ ASTHandler.dataClsModify ctx)
    , ("project.library.ast.data.modify.cons.request"      , call Topic.update $ ASTHandler.dataConsModify ctx)
    , ("project.library.ast.data.modify.methods.request"   , call Topic.update $ ASTHandler.dataMethodsModify ctx)
    , ("project.library.ast.function.add.request"          , call Topic.update $ ASTHandler.functionAdd ctx)
    , ("project.library.ast.function.modify.inputs.request", call Topic.update $ ASTHandler.functionInputsModify ctx)
    , ("project.library.ast.function.modify.name.request"  , call Topic.update $ ASTHandler.functionNameModify ctx)
    , ("project.library.ast.function.modify.output.request", call Topic.update $ ASTHandler.functionOutputModify ctx)
    , ("project.library.ast.function.modify.path.request"  , call Topic.update $ ASTHandler.functionPathModify ctx)
    , ("project.library.ast.function.graph.get.request"               , call Topic.status $ GraphHandler.get ctx)
    , ("project.library.ast.function.graph.connect.request"           , call Topic.update $ GraphHandler.connect ctx)
    , ("project.library.ast.function.graph.disconnect.request"        , call Topic.update $ GraphHandler.disconnect ctx)
    , ("project.library.ast.function.graph.lookup.request"            , call Topic.status $ GraphHandler.lookup ctx)
    , ("project.library.ast.function.graph.node.add.request"          , call Topic.update $ GraphHandler.nodeAdd ctx)
    , ("project.library.ast.function.graph.node.remove.request"       , call Topic.update $ GraphHandler.nodeRemove ctx)
    , ("project.library.ast.function.graph.node.modify.request"       , call Topic.update $ GraphHandler.nodeModify ctx)
    , ("project.library.ast.function.graph.node.modifyinplace.request", call Topic.update $ GraphHandler.nodeModifyInPlace ctx)
    , ("project.library.ast.function.graph.node.default.get.request"   , call Topic.status $ NodeDefaultHandler.get ctx)
    , ("project.library.ast.function.graph.node.default.remove.request", call Topic.update $ NodeDefaultHandler.remove ctx)
    , ("project.library.ast.function.graph.node.default.set.request"   , call Topic.update $ NodeDefaultHandler.set ctx)
    , ("project.library.ast.function.graph.node.properties.get.request", call Topic.status $ PropertiesHandler.getNodeProperties ctx)
    , ("project.library.ast.function.graph.node.properties.set.request", call Topic.update $ PropertiesHandler.setNodeProperties ctx)
    , ("project.library.ast.properties.get.request"                    , call Topic.status $ PropertiesHandler.getASTProperties ctx)
    , ("project.library.ast.properties.set.request"                    , call Topic.update $ PropertiesHandler.setASTProperties ctx)
    ]
    where
        call :: (Proto.Serializable args, Proto.Serializable result)
             => String -> (args -> RPC IO result) -> IO [Message]
        call type_ = callback type_ . Processor.singleResult



