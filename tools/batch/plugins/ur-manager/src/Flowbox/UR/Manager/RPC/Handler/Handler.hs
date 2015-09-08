---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}
--{-# LANGUAGE TemplateHaskell #-}

module Flowbox.UR.Manager.RPC.Handler.Handler where

import Control.Monad.Trans.State

import qualified Flowbox.Batch.Project.Project              as Project
import           Flowbox.Bus.Data.Message                   (CorrelationID, Message)
import qualified Flowbox.Bus.Data.Message                   as Message
import           Flowbox.Bus.Data.Topic                     (Topic, status, (/+))
import           Flowbox.Bus.RPC.HandlerMap                 (HandlerMapWithCid)
import qualified Flowbox.Bus.RPC.HandlerMap                 as HandlerMap
import           Flowbox.Bus.RPC.RPC                        (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor           as Processor
import           Flowbox.Data.Convert
import           Flowbox.Prelude                            hiding (Context, error)
import qualified Flowbox.Text.ProtocolBuffers               as Proto
import           Flowbox.UR.Manager.Context                 as Context
import qualified Flowbox.UR.Manager.RPC.Handler.Maintenance as Maintenance
import qualified Flowbox.UR.Manager.RPC.Handler.URM         as URMHandler
import           Flowbox.UR.Manager.RPC.Topic               as Topic
import qualified Generated.Proto.Bus.Message                as Bus
import qualified Generated.Proto.Urm.URM.Register.Request   as Register


--data Message = RegisterRequest
--             | ...

--data Respond = Status
--             | Update

--handlerMap msg = case msg of
--    RegisterRequest         -> respond $ respond             Status URMHandler.register
--    RegisterMultipleRequest -> respond $ respond             Status URMHandler.register
--    RedoRequest             -> respond $ respondWithFeedback Status URMHandler.register

-- TODO zrefaktoruj funckje by byly przejrzystsze - serialize, a , b, c ?
handlerMap :: HandlerMapWithCid Context IO
handlerMap callback = HandlerMap.fromList
    [ (Topic.urmRegisterRequest          , respondWithAction status         URMHandler.register        )
    , (Topic.urmRegisterMultipleRequest  , respondWithAction status         URMHandler.registerMultiple)
    , (Topic.urmRedoRequest              , respondWithAction status $ const URMHandler.redo            )
    , (Topic.urmPingRequest              , respond           status $ const Maintenance.ping           )
    , (Topic.urmUndoRequest              , respondWithAction status $ const URMHandler.undo            )
    , (Topic.urmClearStackRequest        , respondWithAction status $ const URMHandler.clearStack      )
    , (Topic.urmTransactionBeginRequest  , respond           status         URMHandler.tBegin          )
    , (Topic.urmTransactionCommitRequest , respondWithAction status $ const URMHandler.tCommit         )
    , (Topic.urmUndoDescriptionRequest   , respond           status $ const URMHandler.undoDescriptions)
    , (Topic.urmRedoDescriptionRequest   , respond           status $ const URMHandler.redoDescriptions)
    ]
    where
        respond :: (Proto.Serializable args, Proto.Serializable result)
                => String -> (CorrelationID -> args -> RPC Context IO result) -> StateT Context IO [Message]
        respond type_ = callback (/+ type_) . Processor.singleResultWithCid
        respondWithAction :: (Proto.Serializable request, Proto.Serializable result)
                          => String -> (CorrelationID -> request -> RPC Context IO (result, [Message])) -> StateT Context IO [Message]
        respondWithAction type_ handler = callback (/+ type_) (\cid request -> do (result, action) <- handler cid request
                                                                                  return ([result], action))
