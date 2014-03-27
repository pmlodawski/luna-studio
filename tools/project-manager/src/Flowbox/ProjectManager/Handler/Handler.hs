---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.ProjectManager.Handler.Handler where

import           Flowbox.Bus.RPC.BusRPCHandler              (BusRPCHandler)
import qualified Flowbox.Bus.RPC.Processor                  as P
import           Flowbox.Bus.Topic                          (Topic)
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
logger = getLoggerIO "Flowbox.ProjectManager.Processor"


-- {-# LANGUAGE ImpredicativeTypes #-}
--type HandlerItem = forall args result. (Proto.Serializable args, Proto.Serializable result)
--                 => (Topic, String, args -> IO result)


--handlerItems :: ContextRef -> [HandlerItem]
--handlerItems ctxRef =
--    [ ("project.list.request",   P.status, ProjectHandler.list   ctxRef) ]


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
    "project.list.request"           -> callback P.status $ ProjectHandler.list ctx
    "project.lookup.request"         -> callback P.status $ ProjectHandler.lookup ctx
    "project.create.request"         -> callback P.update $ ProjectHandler.create ctx
    "project.open.request"           -> callback P.update $ ProjectHandler.open ctx
    "project.update.request"         -> callback P.update $ ProjectHandler.update ctx
    "project.close.request"          -> callback P.update $ ProjectHandler.close ctx
    "project.store.request"          -> callback P.status $ ProjectHandler.store ctx
    "project.library.list.request"   -> callback P.status $ LibraryHandler.list ctx
    "project.library.lookup.request" -> callback P.status $ LibraryHandler.lookup ctx
    "project.library.create.request" -> callback P.update $ LibraryHandler.create ctx
    "project.library.load.request"   -> callback P.update $ LibraryHandler.load ctx
    "project.library.unload.request" -> callback P.update $ LibraryHandler.unload ctx
    "project.library.store.request"  -> callback P.status $ LibraryHandler.store ctx
    "project.library.ast.get.request"                    -> callback P.status $ ASTHandler.get ctx
    "project.library.ast.remove.request"                 -> callback P.update $ ASTHandler.remove ctx
    "project.library.ast.resolve.request"                -> callback P.status $ ASTHandler.resolve ctx
    "project.library.ast.module.add.request"             -> callback P.update $ ASTHandler.moduleAdd ctx
    "project.library.ast.module.update.cls.request"      -> callback P.update $ ASTHandler.moduleClsUpdate ctx
    "project.library.ast.module.update.fields.request"   -> callback P.update $ ASTHandler.moduleFieldsUpdate ctx
    "project.library.ast.module.update.imports.request"  -> callback P.update $ ASTHandler.moduleImportsUpdate ctx
    "project.library.ast.data.add.request"               -> callback P.update $ ASTHandler.dataAdd ctx
    "project.library.ast.data.update.classes.request"    -> callback P.update $ ASTHandler.dataClassesUpdate ctx
    "project.library.ast.data.update.cls.request"        -> callback P.update $ ASTHandler.dataClsUpdate ctx
    "project.library.ast.data.update.cons.request"       -> callback P.update $ ASTHandler.dataConsUpdate ctx
    "project.library.ast.data.update.methods.request"    -> callback P.update $ ASTHandler.dataMethodsUpdate ctx
    "project.library.ast.function.add.request"           -> callback P.update $ ASTHandler.functionAdd ctx
    "project.library.ast.function.update.inputs.request" -> callback P.update $ ASTHandler.functionInputsUpdate ctx
    "project.library.ast.function.update.name.request"   -> callback P.update $ ASTHandler.functionNameUpdate ctx
    "project.library.ast.function.update.output.request" -> callback P.update $ ASTHandler.functionOutputUpdate ctx
    "project.library.ast.function.update.path.request"   -> callback P.update $ ASTHandler.functionPathUpdate ctx
    "project.library.ast.function.graph.get.request"                -> callback P.status $ GraphHandler.get ctx
    "project.library.ast.function.graph.connect.request"            -> callback P.update $ GraphHandler.connect ctx
    "project.library.ast.function.graph.disconnect.request"         -> callback P.update $ GraphHandler.disconnect ctx
    "project.library.ast.function.graph.lookup.request"             -> callback P.status $ GraphHandler.lookup ctx
    "project.library.ast.function.graph.node.add.request"           -> callback P.update $ GraphHandler.nodeAdd ctx
    "project.library.ast.function.graph.node.remove.request"        -> callback P.update $ GraphHandler.nodeRemove ctx
    "project.library.ast.function.graph.node.update.request"        -> callback P.update $ GraphHandler.nodeUpdate ctx
    "project.library.ast.function.graph.node.updateInPlace.request" -> callback P.update $ GraphHandler.nodeUpdateInPlace ctx
    "project.library.ast.function.graph.node.default.get.request"    -> callback P.status $ NodeDefaultHandler.get ctx
    "project.library.ast.function.graph.node.default.remove.request" -> callback P.update $ NodeDefaultHandler.remove ctx
    "project.library.ast.function.graph.node.default.set.request"    -> callback P.update $ NodeDefaultHandler.set ctx
    "project.library.ast.properties.get.request"                     -> callback P.update $ PropertiesHandler.get ctx
    "project.library.ast.properties.set.request"                     -> callback P.update $ PropertiesHandler.set ctx
    unsupported             -> do let errMsg = "Unknown topic: " ++ show unsupported
                                  logger error errMsg
                                  return $ P.respondError topic errMsg



