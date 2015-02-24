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

import           Flowbox.Bus.Data.Message                   (Message)
import           Flowbox.Bus.Data.Topic                     (status, (/+))
import           Flowbox.Bus.RPC.HandlerMap                 (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                 as HandlerMap
import           Flowbox.Bus.RPC.RPC                        (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor           as Processor
import qualified Flowbox.UR.Manager.RPC.Handler.Maintenance as Maintenance
import qualified Flowbox.UR.Manager.RPC.Handler.URM         as URMHandler
import           Flowbox.UR.Manager.RPC.Topic               as Topic
import           Flowbox.UR.Manager.Context                 as Context
import           Flowbox.Prelude                            hiding (error, Context)
import qualified Flowbox.Text.ProtocolBuffers               as Proto

handlerMap :: HandlerMap Context IO
handlerMap callback = HandlerMap.fromList
    [ (Topic.urmUndoRegisterRequest, respond status URMHandler.register)
    , (Topic.urmUndoPerformRequest , respond2 status URMHandler.undo)
    , (Topic.urmPingRequest        , respond status Maintenance.ping) 
    ]
    where
        respond :: (Proto.Serializable args, Proto.Serializable result)
                => String -> (args -> RPC Context IO result) -> StateT Context IO [Message]
        respond type_ = callback (/+ type_) . Processor.singleResult
        respond2 :: (Proto.Serializable args, Proto.Serializable result)
                 => String -> (args -> RPC Context IO (result, Maybe Message)) -> StateT Context IO [Message]
        respond2 type_ fun = callback (/+ type_) (\a -> do
                                                          (b, c) <- fun a
                                                          return ([b], maybeToList c))
