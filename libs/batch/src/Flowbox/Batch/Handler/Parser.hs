---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Parser where

import           Flowbox.Luna.Data.AST.Expr                         (Expr)
import           Flowbox.Luna.Data.AST.Pat                          (Pat)
import           Flowbox.Luna.Data.AST.Type                         (Type)
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser as Parser
import           Flowbox.Prelude



parseExpr :: (Applicative m, Monad m) => String -> m Expr
parseExpr str = case Parser.parseExpr str 0 of
    Left  er     -> fail $ show er
    Right (e, _) -> return e


parsePat :: (Applicative m, Monad m) => String -> m Pat
parsePat str = case Parser.parsePattern str 0 of
    Left  er     -> fail $ show er
    Right (e, _) -> return e


parseType :: (Applicative m, Monad m) => String -> m Type
parseType str = case Parser.parseType str 0 of
    Left  er     -> fail $ show er
    Right (e, _) -> return e
