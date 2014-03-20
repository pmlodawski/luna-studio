---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.ProjectManager.Handler.Handler where

import           Flowbox.Bus.RPC.BusRPCHandler          (BusRPCHandler)
import qualified Flowbox.Bus.RPC.Processor              as P
import           Flowbox.Bus.Topic                      (Topic)
import           Flowbox.Prelude                        hiding (error)
import           Flowbox.ProjectManager.Context         (ContextRef)
import qualified Flowbox.ProjectManager.Handler.Library as LibraryHandler
import qualified Flowbox.ProjectManager.Handler.Project as ProjectHandler
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.Processor"


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
         ]


handler :: ContextRef -> BusRPCHandler
handler ctx callback topic = case topic of
    "project.list.request"           -> callback P.status $ ProjectHandler.list   ctx
    "project.lookup.request"         -> callback P.status $ ProjectHandler.lookup ctx
    "project.create.request"         -> callback P.update $ ProjectHandler.create ctx
    "project.open.request"           -> callback P.update $ ProjectHandler.open   ctx
    "project.update.request"         -> callback P.update $ ProjectHandler.update ctx
    "project.close.request"          -> callback P.update $ ProjectHandler.close  ctx
    "project.store.request"          -> callback P.status $ ProjectHandler.store  ctx
    "project.library.list.request"   -> callback P.status $ LibraryHandler.list   ctx
    "project.library.lookup.request" -> callback P.status $ LibraryHandler.lookup ctx
    "project.library.create.request" -> callback P.update $ LibraryHandler.create ctx
    "project.library.load.request"   -> callback P.update $ LibraryHandler.load   ctx
    "project.library.unload.request" -> callback P.update $ LibraryHandler.unload ctx
    "project.library.store.request"  -> callback P.status $ LibraryHandler.store  ctx
    unsupported             -> do let errMsg = "Unknown topic: " ++ show unsupported
                                  logger error errMsg
                                  return $ P.respondError topic errMsg
