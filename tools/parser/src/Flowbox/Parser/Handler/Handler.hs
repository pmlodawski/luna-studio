---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Parser.Handler.Handler where

import           Flowbox.Bus.Data.Topic        (Topic)
import           Flowbox.Bus.RPC.BusRPCHandler (BusRPCHandler)
import qualified Flowbox.Bus.RPC.Processor     as P
import qualified Flowbox.Parser.Handler.Parser as ParserHandler
import           Flowbox.Prelude               hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Parser.Handler"


topics :: [Topic]
topics = [ "parse.expr.request"
         , "parse.pat.request"
         , "parse.type.request"
         , "parse.nodeexpr.request"
         ]


handler :: BusRPCHandler
handler  callback topic = case topic of
    "parse.expr.request"     -> callback P.status $ P.singleResult ParserHandler.parseExpr
    "parse.pat.request"      -> callback P.status $ P.singleResult ParserHandler.parsePat
    "parse.type.request"     -> callback P.status $ P.singleResult ParserHandler.parseType
    "parse.nodeexpr.request" -> callback P.status $ P.singleResult ParserHandler.parseNodeExpr
    unsupported -> do let errMsg = "Unknown topic: " ++ show unsupported
                      logger error errMsg
                      return $ P.respondError topic errMsg
