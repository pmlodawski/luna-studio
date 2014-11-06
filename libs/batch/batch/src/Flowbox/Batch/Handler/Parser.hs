---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Batch.Handler.Parser where

import           Flowbox.Batch.Batch   (Error)
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Luna.AST.Expr         (Expr)
import qualified Luna.AST.Expr         as Expr
import           Luna.AST.Pat          (Pat)
import           Luna.AST.Type         (Type)
import qualified Luna.Data.ASTInfo     as ASTInfo
import qualified Luna.Data.Config      as Config
import qualified Luna.Parser.Parser    as Parser
import qualified Luna.Parser.Token     as Tok

--FIXME[wd]: following imports should be removed after moving to plugin based structure
--           including all use cases. Nothing should modify Parser.State explicitly!
import qualified Luna.Parser.Pragma as Pragma
import qualified Luna.Parser.State  as ParserState
import           Luna.Pragma.Pragma (Pragma)



parseNodeExpr :: (Applicative m, Monad m) => String -> EitherT Error m Expr
parseNodeExpr str = if length str == 1 && head str `elem` Tok.opChars
    -- FIXME [PM] : remove this hack, create real node expression parser
    then return $ Expr.Infix 0 str (Expr.Wildcard 1) (Expr.Wildcard 2)
    else case Parser.parseString str $ Parser.exprParser (patchedParserState $ ASTInfo.mk 0) of
        Left  er     -> left $ show er
        Right (e, _) -> return e


parseExpr :: (Applicative m, Monad m) => String -> EitherT Error m Expr
parseExpr str = case Parser.parseString str $ Parser.exprParser (patchedParserState $ ASTInfo.mk 0) of
    Left  er     -> left $ show er
    Right (e, _) -> return e


parsePat :: (Applicative m, Monad m) => String -> EitherT Error m Pat
parsePat str = case Parser.parseString str $ Parser.patternParser (patchedParserState $ ASTInfo.mk 0) of
    Left  er     -> left $ show er
    Right (e, _) -> return e


parseType :: (Applicative m, Monad m) => String -> EitherT Error m Type
parseType str = case Parser.parseString str $ Parser.typeParser (patchedParserState $ ASTInfo.mk 0) of
    Left  er     -> left $ show er
    Right (e, _) -> return e


patchedParserState :: ASTInfo.ASTInfo
                   -> ParserState.State (Pragma Pragma.ImplicitSelf, (Pragma Pragma.AllowOrphans, (Pragma Pragma.TabLength, ())))
patchedParserState info = def
    & ParserState.info .~ info
    & ParserState.conf .~ parserConf
    where parserConf  = Parser.defConfig & Config.setPragma Pragma.AllowOrphans
