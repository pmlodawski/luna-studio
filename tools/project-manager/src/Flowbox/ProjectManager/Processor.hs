---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.ProjectManager.Processor where

import qualified Data.List         as List
import qualified Data.String.Utils as Utils

import           Flowbox.Bus.Data.Exception                     (Exception (Exception))
import           Flowbox.Bus.Message                            (Message (Message))
import qualified Flowbox.Bus.Message                            as Message
import           Flowbox.Bus.Topic                              (Topic)
import           Flowbox.Control.Error                          hiding (err)
import           Flowbox.Prelude                                hiding (Context, error)
import           Flowbox.ProjectManager.Context                 (Context)
import qualified Flowbox.ProjectManager.Handler.Library         as LibraryHandler
import qualified Flowbox.ProjectManager.Handler.Project         as ProjectHandler
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                   as Proto
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic



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


process :: Context -> Message -> IO Message
process context msg = case topic of
    "project.list.request"   -> call status ProjectHandler.list
    "project.lookup.request" -> call status ProjectHandler.lookup
    "project.create.request" -> call update ProjectHandler.create
    "project.open.request"   -> call update ProjectHandler.open
    "project.update.request" -> call update ProjectHandler.update
    "project.close.request"  -> call update ProjectHandler.close
    "project.store.request"  -> call status ProjectHandler.store
    "project.library.list.request"   -> call status LibraryHandler.list
    "project.library.lookup.request" -> call status LibraryHandler.lookup
    "project.library.create.request" -> call update LibraryHandler.create
    "project.library.load.request"   -> call update LibraryHandler.load
    "project.library.unload.request" -> call update LibraryHandler.unload
    "project.library.store.request"  -> call status LibraryHandler.store
    unsupported              -> do let errMsg = "Unknown topic: " ++ show unsupported
                                   logger error errMsg
                                   return $ respondError errMsg
    where
        call type_ method = case Proto.messageGet' $ Message.message msg of
            Left err   -> do logger error err
                             return $ respondError err
            Right args -> do result <- runEitherT $ scriptIO $ method context args
                             return $ case result of
                                Left err -> respondError err
                                Right ok -> respond type_ ok

        topic = Message.topic msg

        status = "status"
        update = "update"

        respond :: Proto.Serializable msg => String -> msg -> Message
        respond = constructMessage topic

        respondError :: String -> Message
        respondError = respond "error" . encodeP . Exception . Just


constructMessage :: Proto.Serializable msg => Topic -> String ->  msg -> Message
constructMessage topic type_  data_ = Message newTopic $ Proto.messagePut' data_ where
    newTopic = (List.intercalate "." . (++) [type_] . init . Utils.split ".") topic

