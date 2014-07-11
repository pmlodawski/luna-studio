---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Batch.Handler.Parser where

import           Flowbox.Batch.Batch                                (Error)
import           Flowbox.Control.Error
import           Flowbox.Luna.Data.AST.Expr                         (Expr)
import qualified Flowbox.Luna.Data.AST.Expr                         as Expr
import           Flowbox.Luna.Data.AST.Pat                          (Pat)
import           Flowbox.Luna.Data.AST.Type                         (Type)
import qualified Flowbox.Luna.Data.Pass.ASTInfo                     as ASTInfo
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Lexer  as Lexer
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser as Parser
import           Flowbox.Prelude



parseNodeExpr :: (Applicative m, Monad m) => String -> EitherT Error m Expr
parseNodeExpr str = if length str == 1 && head str `elem` Lexer.operators
    then return $ Expr.Infix 0 str (Expr.Wildcard 1) (Expr.Wildcard 2)
    else case Parser.parseExpr str $ ASTInfo.mk 0 of
        Left  er     -> left $ show er
        Right (e, _) -> return e


parseExpr :: (Applicative m, Monad m) => String -> EitherT Error m Expr
parseExpr str = case Parser.parseExpr str $ ASTInfo.mk 0 of
    Left  er     -> left $ show er
    Right (e, _) -> return e


parsePat :: (Applicative m, Monad m) => String -> EitherT Error m Pat
parsePat str = case Parser.parsePattern str $ ASTInfo.mk 0 of
    Left  er     -> left $ show er
    Right (e, _) -> return e


parseType :: (Applicative m, Monad m) => String -> EitherT Error m Type
parseType str = case Parser.parseType str $ ASTInfo.mk 0 of
    Left  er     -> left $ show er
    Right (e, _) -> return e

