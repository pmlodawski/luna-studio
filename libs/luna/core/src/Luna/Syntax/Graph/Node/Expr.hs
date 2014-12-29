---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Syntax.Graph.Node.Expr where

import Flowbox.Prelude
import Luna.Syntax.Expr                  (Expr)
import Luna.Syntax.Graph.Node.StringExpr (StringExpr)



data NodeExpr a v = StringExpr { _strExpr :: StringExpr }
                  | ASTExpr    { _expr :: Expr a v      }
                  deriving (Show, Eq, Read)



makeLenses ''NodeExpr
