---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.UR.Manager.RPC.Handler.Handler where

import Control.Monad.Trans.State
import           Data.Maybe      (maybeToList)

import qualified Flowbox.Batch.Project.Project              as Project
import           Flowbox.Bus.Data.Message                   (Message)
import qualified Flowbox.Bus.Data.Message                   as Message
import           Flowbox.Bus.Data.Topic                     (Topic)
import           Flowbox.Bus.Data.Topic                     (status, (/+))
import           Flowbox.Bus.RPC.HandlerMap                 (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                 as HandlerMap
import           Flowbox.Bus.RPC.RPC                        (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor           as Processor
import           Flowbox.Data.Convert
import           Flowbox.Prelude                            hiding (error, Context)
import qualified Flowbox.Text.ProtocolBuffers               as Proto
import           Flowbox.UR.Manager.Context                 as Context
import qualified Flowbox.UR.Manager.RPC.Handler.Maintenance as Maintenance
import qualified Flowbox.UR.Manager.RPC.Handler.URM         as URMHandler
import           Flowbox.UR.Manager.RPC.Topic               as Topic
import qualified Generated.Proto.Bus.Message                as Bus
import qualified Generated.Proto.Urm.URM.Register.Request   as Register

handlerMap :: HandlerMap Context IO
handlerMap callback = HandlerMap.fromList
    [ (Topic.urmRegisterRequest          , respond status URMHandler.register)
    , (Topic.urmRegisterMultipleRequest  , respond status URMHandler.registerMultiple)
    , (Topic.urmRedoRequest              , respond2 status URMHandler.redo)
    , (Topic.urmPingRequest              , respond status Maintenance.ping) 
    , (Topic.urmUndoRequest              , respond2 status URMHandler.undo)
    , (Topic.urmClearStackRequest        , respond status URMHandler.clearStack)
    , (Topic.urmTransactionBeginRequest  , respond status URMHandler.tBegin)
    , (Topic.urmTransactionCommitRequest , respond status URMHandler.tCommit)
    ]
    where
        respond :: (Proto.Serializable args, Proto.Serializable result)
                => String -> (args -> RPC Context IO result) -> StateT Context IO [Message]
        respond type_ = callback (/+ type_) . Processor.singleResult
        respond2 :: (Proto.Serializable args, Proto.Serializable result)
                 => String -> (args -> RPC Context IO (result, Maybe [Message])) -> StateT Context IO [Message]
        respond2 type_ fun = callback (/+ type_) (\a -> do (b, c) <- fun a
                                                           return ([b], concat $ maybeToList c))


fun :: Proto.Serializable message => Topic -> message -> Bus.Message
fun = (encodeP .) . Message.mk . ("undone." ++)


makeMsgArr :: (Proto.ReflectDescriptor request, Proto.Wire request) => request -> Maybe Topic -> [Message]
makeMsgArr request = maybe [] $ return . (flip Message.mk request)


prepareResponse :: (Proto.Serializable undoMessage, Proto.Serializable redoMessage, Proto.Serializable urmMessage, Monad m)
                => Project.ID -> Topic -> undoMessage -> Topic -> redoMessage -> Maybe Topic -> String -> urmMessage -> m ([urmMessage], [Message])
prepareResponse projectID undoTopic undoAction redoTopic redoAction urmTopic description = return . (flip (,) urmMessages . return)
    where
        urmMessages = makeMsgArr (Register.Request (fun undoTopic $ undoAction)
                                                   (fun redoTopic $ redoAction)
                                                   (encodeP projectID)
                                                   (encodeP description)
                                 ) urmTopic
