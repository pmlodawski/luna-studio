---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Parser.Handler.Handler where

import           Flowbox.Bus.Data.Message         (Message)
import qualified Flowbox.Bus.Data.Topic           as Topic
import           Flowbox.Bus.RPC.HandlerMap       (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap       as HandlerMap
import           Flowbox.Bus.RPC.RPC              (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor as Processor
import qualified Flowbox.Parser.Handler.Parser    as ParserHandler
import           Flowbox.Prelude                  hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers     as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Parser.Handler.Handler"


handlerMap :: HandlerMap IO
handlerMap callback = HandlerMap.fromList
    [ ("parse.expr.request"    , call Topic.status ParserHandler.parseExpr    )
    , ("parse.pat.request"     , call Topic.status ParserHandler.parsePat     )
    , ("parse.type.request"    , call Topic.status ParserHandler.parseType    )
    , ("parse.nodeexpr.request", call Topic.status ParserHandler.parseNodeExpr)
    ]
    where
        call :: (Proto.Serializable args, Proto.Serializable result)
             => String -> (args -> RPC IO result) -> IO [Message]
        call type_ = callback type_ . Processor.singleResult
