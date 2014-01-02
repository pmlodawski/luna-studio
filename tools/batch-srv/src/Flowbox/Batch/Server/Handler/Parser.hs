---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handler.Parser where

import Data.IORef (IORef)

import           Flowbox.Batch.Batch                                (Batch)
import qualified Flowbox.Batch.Handler.Parser                       as BatchP
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Expr ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Pat  ()
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Batch.Parser.ParseExpr.Args        as ParseExpr
import qualified Generated.Proto.Batch.Parser.ParseExpr.Result      as ParseExpr
import qualified Generated.Proto.Batch.Parser.ParsePat.Args         as ParsePat
import qualified Generated.Proto.Batch.Parser.ParsePat.Result       as ParsePat
import qualified Generated.Proto.Batch.Parser.ParseType.Args        as ParseType
import qualified Generated.Proto.Batch.Parser.ParseType.Result      as ParseType



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.Parser"

-------- public api -------------------------------------------------

parseExpr :: IORef Batch -> ParseExpr.Args -> IO ParseExpr.Result
parseExpr _ (ParseExpr.Args tstr) = do
    loggerIO info "called parseExpr"
    let str = decodeP tstr
    loggerIO debug $ "str: " ++ (show str)
    expr <- BatchP.parseExpr str
    return $ ParseExpr.Result $ encode expr


parsePat :: IORef Batch -> ParsePat.Args -> IO ParsePat.Result
parsePat _ (ParsePat.Args tstr) = do
    loggerIO info "called parsePat"
    let str = decodeP tstr
    loggerIO debug $ "str: " ++ (show str)
    pat <- BatchP.parsePat str
    return $ ParsePat.Result $ encode pat


parseType :: IORef Batch -> ParseType.Args -> IO ParseType.Result
parseType _ (ParseType.Args tstr) = do
    loggerIO info "called parseType"
    let str = decodeP tstr
    loggerIO debug $ "str: " ++ (show str)
    pat <- BatchP.parseType str
    return $ ParseType.Result $ encode pat
