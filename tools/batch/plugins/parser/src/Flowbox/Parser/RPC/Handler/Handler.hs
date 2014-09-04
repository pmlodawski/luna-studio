---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Parser.RPC.Handler.Handler where

import Control.Monad.Trans.State

import           Flowbox.Bus.Data.Message          (Message)
import           Flowbox.Bus.Data.Topic            (status, (/+))
import           Flowbox.Bus.RPC.HandlerMap        (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap        as HandlerMap
import           Flowbox.Bus.RPC.RPC               (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor  as Processor
import qualified Flowbox.Parser.RPC.Handler.Parser as ParserHandler
import           Flowbox.Prelude                   hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers      as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Parser.RPC.Handler.Handler"


handlerMap :: HandlerMap () IO
handlerMap callback = HandlerMap.fromList
    [ ("parse.expr.request"    , respond status ParserHandler.parseExpr    )
    , ("parse.pat.request"     , respond status ParserHandler.parsePat     )
    , ("parse.type.request"    , respond status ParserHandler.parseType    )
    , ("parse.nodeexpr.request", respond status ParserHandler.parseNodeExpr)
    , ("parser.ping.request"   , respond status ParserHandler.ping)
    ]
    where
        respond :: (Proto.Serializable args, Proto.Serializable result)
                => String -> (args -> RPC () IO result) -> StateT () IO [Message]
        respond type_ = callback (/+ type_) . Processor.singleResult
