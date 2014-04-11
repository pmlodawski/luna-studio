---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.ProjectManager.Handler.Handler where

import           Flowbox.Bus.Data.Topic                     (Topic)
import           Flowbox.Bus.RPC.BusRPCHandler              (BusRPCHandler)
import qualified Flowbox.Bus.RPC.Processor                  as P
import           Flowbox.Prelude                            hiding (error)
import           Flowbox.ProjectManager.Context             (ContextRef)
import qualified Flowbox.ProjectManager.Handler.AST         as ASTHandler
import qualified Flowbox.ProjectManager.Handler.Graph       as GraphHandler
import qualified Flowbox.ProjectManager.Handler.Library     as LibraryHandler
import qualified Flowbox.ProjectManager.Handler.NodeDefault as NodeDefaultHandler
import qualified Flowbox.ProjectManager.Handler.Project     as ProjectHandler
import qualified Flowbox.ProjectManager.Handler.Properties  as PropertiesHandler
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.Handler"


-- {-# LANGUAGE ImpredicativeTypes #-}
--type HandlerItem = forall args result. (Proto.Serializable args, Proto.Serializable result)
--                 => (Topic, String, args -> IO result)


--handlerItems :: ContextRef -> [HandlerItem]
--handlerItems ctxRef =
--    [ ("project.list.request",   P.status $ P.singleResult , ProjectHandler.list   ctxRef) ]


topics :: [Topic]
topics = [ "project.list.request"
         , "project.lookup.request"
         , "project.create.request"
         , "project.open.request"
         , "project.modify.request"
         , "project.close.request"
         , "project.store.request"
         , "project.library.list.request"
         , "project.library.lookup.request"
         , "project.library.create.request"
         , "project.library.load.request"
         , "project.library.unload.request"
         , "project.library.store.request"
         , "project.library.ast.get.request"
         , "project.library.ast.remove.request"
         , "project.library.ast.resolve.request"
         , "project.library.ast.module.add.request"
         , "project.library.ast.module.modify.cls.request"
         , "project.library.ast.module.modify.fields.request"
         , "project.library.ast.module.modify.imports.request"
         , "project.library.ast.data.add.request"
         , "project.library.ast.data.modify.classes.request"
         , "project.library.ast.data.modify.cls.request"
         , "project.library.ast.data.modify.cons.request"
         , "project.library.ast.data.modify.methods.request"
         , "project.library.ast.function.add.request"
         , "project.library.ast.function.modify.inputs.request"
         , "project.library.ast.function.modify.name.request"
         , "project.library.ast.function.modify.output.request"
         , "project.library.ast.function.modify.path.request"
         , "project.library.ast.function.graph.get.request"
         , "project.library.ast.function.graph.connect.request"
         , "project.library.ast.function.graph.disconnect.request"
         , "project.library.ast.function.graph.lookup.request"
         , "project.library.ast.function.graph.node.add.request"
         , "project.library.ast.function.graph.node.remove.request"
         , "project.library.ast.function.graph.node.modify.request"
         , "project.library.ast.function.graph.node.updateInPlace.request"
         , "project.library.ast.function.graph.node.default.get.request"
         , "project.library.ast.function.graph.node.default.remove.request"
         , "project.library.ast.function.graph.node.default.set.request"
         , "project.library.ast.properties.get.request"
         , "project.library.ast.properties.set.request"
         ]


handler :: ContextRef -> BusRPCHandler
handler ctx callback topic = case topic of
    "project.list.request"           -> callback P.status $ P.singleResult $ ProjectHandler.list ctx
    "project.lookup.request"         -> callback P.status $ P.singleResult $ ProjectHandler.lookup ctx
    "project.create.request"         -> callback P.update $ P.singleResult $ ProjectHandler.create ctx
    "project.open.request"           -> callback P.update $ P.singleResult $ ProjectHandler.open ctx
    "project.modify.request"         -> callback P.update $ P.singleResult $ ProjectHandler.modify ctx
    "project.close.request"          -> callback P.update $ P.singleResult $ ProjectHandler.close ctx
    "project.store.request"          -> callback P.status $ P.singleResult $ ProjectHandler.store ctx
    "project.library.list.request"   -> callback P.status $ P.singleResult $ LibraryHandler.list ctx
    "project.library.lookup.request" -> callback P.status $ P.singleResult $ LibraryHandler.lookup ctx
    "project.library.create.request" -> callback P.update $ P.singleResult $ LibraryHandler.create ctx
    "project.library.load.request"   -> callback P.update $ P.singleResult $ LibraryHandler.load ctx
    "project.library.unload.request" -> callback P.update $ P.singleResult $ LibraryHandler.unload ctx
    "project.library.store.request"  -> callback P.status $ P.singleResult $ LibraryHandler.store ctx
    "project.library.ast.get.request"                    -> callback P.status $ P.singleResult $ ASTHandler.get ctx
    "project.library.ast.remove.request"                 -> callback P.update $ P.singleResult $ ASTHandler.remove ctx
    "project.library.ast.resolve.request"                -> callback P.status $ P.singleResult $ ASTHandler.resolve ctx
    "project.library.ast.module.add.request"             -> callback P.update $ P.singleResult $ ASTHandler.moduleAdd ctx
    "project.library.ast.module.modify.cls.request"      -> callback P.update $ P.singleResult $ ASTHandler.moduleClsModify ctx
    "project.library.ast.module.modify.fields.request"   -> callback P.update $ P.singleResult $ ASTHandler.moduleFieldsModify ctx
    "project.library.ast.module.modify.imports.request"  -> callback P.update $ P.singleResult $ ASTHandler.moduleImportsModify ctx
    "project.library.ast.data.add.request"               -> callback P.update $ P.singleResult $ ASTHandler.dataAdd ctx
    "project.library.ast.data.modify.classes.request"    -> callback P.update $ P.singleResult $ ASTHandler.dataClassesModify ctx
    "project.library.ast.data.modify.cls.request"        -> callback P.update $ P.singleResult $ ASTHandler.dataClsModify ctx
    "project.library.ast.data.modify.cons.request"       -> callback P.update $ P.singleResult $ ASTHandler.dataConsModify ctx
    "project.library.ast.data.modify.methods.request"    -> callback P.update $ P.singleResult $ ASTHandler.dataMethodsModify ctx
    "project.library.ast.function.add.request"           -> callback P.update $ P.singleResult $ ASTHandler.functionAdd ctx
    "project.library.ast.function.modify.inputs.request" -> callback P.update $ P.singleResult $ ASTHandler.functionInputsModify ctx
    "project.library.ast.function.modify.name.request"   -> callback P.update $ P.singleResult $ ASTHandler.functionNameModify ctx
    "project.library.ast.function.modify.output.request" -> callback P.update $ P.singleResult $ ASTHandler.functionOutputModify ctx
    "project.library.ast.function.modify.path.request"   -> callback P.update $ P.singleResult $ ASTHandler.functionPathModify ctx
    "project.library.ast.function.graph.get.request"                -> callback P.status $ P.singleResult $ GraphHandler.get ctx
    "project.library.ast.function.graph.connect.request"            -> callback P.update $ P.singleResult $ GraphHandler.connect ctx
    "project.library.ast.function.graph.disconnect.request"         -> callback P.update $ P.singleResult $ GraphHandler.disconnect ctx
    "project.library.ast.function.graph.lookup.request"             -> callback P.status $ P.singleResult $ GraphHandler.lookup ctx
    "project.library.ast.function.graph.node.add.request"           -> callback P.update $ P.singleResult $ GraphHandler.nodeAdd ctx
    "project.library.ast.function.graph.node.remove.request"        -> callback P.update $ P.singleResult $ GraphHandler.nodeRemove ctx
    "project.library.ast.function.graph.node.modify.request"        -> callback P.update $ P.singleResult $ GraphHandler.nodeModify ctx
    "project.library.ast.function.graph.node.updateInPlace.request" -> callback P.update $ P.singleResult $ GraphHandler.nodeModifyInPlace ctx
    "project.library.ast.function.graph.node.default.get.request"    -> callback P.status $ P.singleResult $ NodeDefaultHandler.get ctx
    "project.library.ast.function.graph.node.default.remove.request" -> callback P.update $ P.singleResult $ NodeDefaultHandler.remove ctx
    "project.library.ast.function.graph.node.default.set.request"    -> callback P.update $ P.singleResult $ NodeDefaultHandler.set ctx
    "project.library.ast.properties.get.request"                     -> callback P.update $ P.singleResult $ PropertiesHandler.get ctx
    "project.library.ast.properties.set.request"                     -> callback P.update $ P.singleResult $ PropertiesHandler.set ctx
    unsupported             -> do let errMsg = "Unknown topic: " ++ show unsupported
                                  logger error errMsg
                                  return $ P.respondError topic errMsg



