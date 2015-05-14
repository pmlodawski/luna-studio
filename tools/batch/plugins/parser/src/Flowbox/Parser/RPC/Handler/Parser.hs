---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Flowbox.Parser.RPC.Handler.Parser where

import qualified Flowbox.Batch.Handler.Parser                   as BatchP
import           Flowbox.Bus.RPC.RPC                            (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Generated.Proto.Parser.MkText.Request          as MkText
import qualified Generated.Proto.Parser.MkText.Status           as MkText
import qualified Generated.Proto.Parser.Parse.Expr.Request      as ParseExpr
import qualified Generated.Proto.Parser.Parse.Expr.Status       as ParseExpr
import qualified Generated.Proto.Parser.Parse.NodeExpr.Request  as ParseNodeExpr
import qualified Generated.Proto.Parser.Parse.NodeExpr.Status   as ParseNodeExpr
import qualified Generated.Proto.Parser.Parse.Pat.Request       as ParsePat
import qualified Generated.Proto.Parser.Parse.Pat.Status        as ParsePat
import qualified Generated.Proto.Parser.Parse.Type.Request      as ParseType
import qualified Generated.Proto.Parser.Parse.Type.Status       as ParseType
import qualified Generated.Proto.Parser.Parser.Ping.Request     as Ping
import qualified Generated.Proto.Parser.Parser.Ping.Status      as Ping
import           Luna.DEP.AST.Expr                              (Expr)
import           Luna.DEP.Data.Serialize.Proto.Conversion.Crumb ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Expr  ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Pat   ()
import           Luna.DEP.Util.LunaShow                         (lunaShow)


logger :: LoggerIO
logger = getLoggerIO $moduleName

-------- public api -------------------------------------------------

parseExpr :: ParseExpr.Request -> RPC () IO ParseExpr.Status
parseExpr request@(ParseExpr.Request tstr) = do
    let str = decodeP tstr
    expr <- BatchP.parseExpr str
    return $ ParseExpr.Status request (encode expr)


parsePat :: ParsePat.Request -> RPC () IO ParsePat.Status
parsePat request@(ParsePat.Request tstr) = do
    let str = decodeP tstr
    pat <- BatchP.parsePat str
    return $ ParsePat.Status request(encode pat)


parseType :: ParseType.Request -> RPC () IO ParseType.Status
parseType request@(ParseType.Request tstr) = do
    let str = decodeP tstr
    pat <- BatchP.parseType str
    return $ ParseType.Status request (encode pat)


parseNodeExpr :: ParseNodeExpr.Request -> RPC () IO ParseNodeExpr.Status
parseNodeExpr request@(ParseNodeExpr.Request tstr) = do
    let str = decodeP tstr
    expr <- BatchP.parseNodeExpr str
    return $ ParseNodeExpr.Status request (encode expr)


mkText :: MkText.Request -> RPC () IO MkText.Status
mkText request@(MkText.Request texpr) = do
    expr <- decodeE texpr
    let text = lunaShow (expr :: Expr)
    seq text $ return ()
    return $ MkText.Status request $ encodeP text


ping :: Ping.Request -> RPC () IO Ping.Status
ping request = do
    logger info "Ping received"
    return $ Ping.Status request
