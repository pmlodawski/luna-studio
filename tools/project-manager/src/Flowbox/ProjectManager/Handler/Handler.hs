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
         , "project.update.request"
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
         , "project.library.ast.module.update.cls.request"
         , "project.library.ast.module.update.fields.request"
         , "project.library.ast.module.update.imports.request"
         , "project.library.ast.data.add.request"
         , "project.library.ast.data.update.classes.request"
         , "project.library.ast.data.update.cls.request"
         , "project.library.ast.data.update.cons.request"
         , "project.library.ast.data.update.methods.request"
         , "project.library.ast.function.add.request"
         , "project.library.ast.function.update.inputs.request"
         , "project.library.ast.function.update.name.request"
         , "project.library.ast.function.update.output.request"
         , "project.library.ast.function.update.path.request"
         , "project.library.ast.function.graph.get.request"
         , "project.library.ast.function.graph.connect.request"
         , "project.library.ast.function.graph.disconnect.request"
         , "project.library.ast.function.graph.lookup.request"
         , "project.library.ast.function.graph.node.add.request"
         , "project.library.ast.function.graph.node.remove.request"
         , "project.library.ast.function.graph.node.update.request"
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
    "project.update.request"         -> callback P.update $ P.singleResult $ ProjectHandler.update ctx
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
    "project.library.ast.module.update.cls.request"      -> callback P.update $ P.singleResult $ ASTHandler.moduleClsUpdate ctx
    "project.library.ast.module.update.fields.request"   -> callback P.update $ P.singleResult $ ASTHandler.moduleFieldsUpdate ctx
    "project.library.ast.module.update.imports.request"  -> callback P.update $ P.singleResult $ ASTHandler.moduleImportsUpdate ctx
    "project.library.ast.data.add.request"               -> callback P.update $ P.singleResult $ ASTHandler.dataAdd ctx
    "project.library.ast.data.update.classes.request"    -> callback P.update $ P.singleResult $ ASTHandler.dataClassesUpdate ctx
    "project.library.ast.data.update.cls.request"        -> callback P.update $ P.singleResult $ ASTHandler.dataClsUpdate ctx
    "project.library.ast.data.update.cons.request"       -> callback P.update $ P.singleResult $ ASTHandler.dataConsUpdate ctx
    "project.library.ast.data.update.methods.request"    -> callback P.update $ P.singleResult $ ASTHandler.dataMethodsUpdate ctx
    "project.library.ast.function.add.request"           -> callback P.update $ P.singleResult $ ASTHandler.functionAdd ctx
    "project.library.ast.function.update.inputs.request" -> callback P.update $ P.singleResult $ ASTHandler.functionInputsUpdate ctx
    "project.library.ast.function.update.name.request"   -> callback P.update $ P.singleResult $ ASTHandler.functionNameUpdate ctx
    "project.library.ast.function.update.output.request" -> callback P.update $ P.singleResult $ ASTHandler.functionOutputUpdate ctx
    "project.library.ast.function.update.path.request"   -> callback P.update $ P.singleResult $ ASTHandler.functionPathUpdate ctx
    "project.library.ast.function.graph.get.request"                -> callback P.status $ P.singleResult $ GraphHandler.get ctx
    "project.library.ast.function.graph.connect.request"            -> callback P.update $ P.singleResult $ GraphHandler.connect ctx
    "project.library.ast.function.graph.disconnect.request"         -> callback P.update $ P.singleResult $ GraphHandler.disconnect ctx
    "project.library.ast.function.graph.lookup.request"             -> callback P.status $ P.singleResult $ GraphHandler.lookup ctx
    "project.library.ast.function.graph.node.add.request"           -> callback P.update $ P.singleResult $ GraphHandler.nodeAdd ctx
    "project.library.ast.function.graph.node.remove.request"        -> callback P.update $ P.singleResult $ GraphHandler.nodeRemove ctx
    "project.library.ast.function.graph.node.update.request"        -> callback P.update $ P.singleResult $ GraphHandler.nodeUpdate ctx
    "project.library.ast.function.graph.node.updateInPlace.request" -> callback P.update $ P.singleResult $ GraphHandler.nodeUpdateInPlace ctx
    "project.library.ast.function.graph.node.default.get.request"    -> callback P.status $ P.singleResult $ NodeDefaultHandler.get ctx
    "project.library.ast.function.graph.node.default.remove.request" -> callback P.update $ P.singleResult $ NodeDefaultHandler.remove ctx
    "project.library.ast.function.graph.node.default.set.request"    -> callback P.update $ P.singleResult $ NodeDefaultHandler.set ctx
    "project.library.ast.properties.get.request"                     -> callback P.update $ P.singleResult $ PropertiesHandler.get ctx
    "project.library.ast.properties.set.request"                     -> callback P.update $ P.singleResult $ PropertiesHandler.set ctx
    unsupported             -> do let errMsg = "Unknown topic: " ++ show unsupported
                                  logger error errMsg
                                  return $ P.respondError topic errMsg



