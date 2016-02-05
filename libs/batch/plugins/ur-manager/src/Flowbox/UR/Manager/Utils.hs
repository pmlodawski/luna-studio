---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}

module Flowbox.UR.Manager.Utils where

import qualified Flowbox.Batch.Project.Project                       as Project
import           Flowbox.Bus.Data.Message                            (Message)
import qualified Flowbox.Bus.Data.Message                            as Message
import qualified Flowbox.Bus.Data.Serialize.Proto.Conversion.Message ()
import           Flowbox.Bus.Data.Topic                              (Topic)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                     hiding (Context, error)
import qualified Flowbox.Text.ProtocolBuffers                        as Proto
import qualified Generated.Proto.Bus.Message                         as Bus
import qualified Generated.Proto.Urm.URM.Register.Request            as Register

serialize :: Proto.Serializable message => Topic -> message -> Bus.Message
serialize = (encodeP .) . Message.mk


makeMsgArr :: (Proto.ReflectDescriptor request, Proto.Wire request) => request -> Maybe Topic -> [Message]
makeMsgArr request = maybe [] $ return . flip Message.mk request


prepareResponse :: (Proto.Serializable undoMessage, Proto.Serializable redoMessage, Proto.Serializable urmMessage, Monad m)
                => Project.ID -> Topic -> undoMessage -> Topic -> redoMessage -> Maybe Topic -> String -> urmMessage -> m ([urmMessage], [Message])
prepareResponse projectID undoTopic undoAction redoTopic redoAction urmTopic description = return . flip (,) urmMessages . return
    where
        urmMessages = makeMsgArr (Register.Request (serialize ("undone." ++ undoTopic) undoAction)
                                                   (serialize ("undone." ++ redoTopic) redoAction)
                                                   (encodeP projectID)
                                                   (encodeP description)
                                 ) urmTopic
