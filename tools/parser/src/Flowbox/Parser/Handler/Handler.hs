---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Parser.Handler.Handler where

import           Flowbox.Bus.RPC.BusRPCHandler          (BusRPCHandler)
import qualified Flowbox.Bus.RPC.Processor              as P
import           Flowbox.Bus.Topic                      (Topic)
import qualified Flowbox.Parser.Handler.Parser as ParserHandler
import           Flowbox.Prelude                        hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Parser.Handler"


topics :: [Topic]
topics = [ "parser.parse.expr.request"     
         , "parser.parse.pat.request"      
         , "parser.parse.type.request"     
         , "parser.parse.nodeexpr.request"
         ]


handler :: BusRPCHandler
handler  callback topic = case topic of
    "parser.parse.expr.request"     -> callback P.update ParserHandler.parseExpr
    "parser.parse.pat.request"      -> callback P.update ParserHandler.parsePat
    "parser.parse.type.request"     -> callback P.update ParserHandler.parseType
    "parser.parse.nodeexpr.request" -> callback P.update ParserHandler.parseNodeExpr
    unsupported -> do let errMsg = "Unknown topic: " ++ show unsupported
                      logger error errMsg
                      return $ P.respondError topic errMsg
