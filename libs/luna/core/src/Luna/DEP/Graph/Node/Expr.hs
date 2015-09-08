---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.DEP.Graph.Node.Expr where

import Flowbox.Prelude
import Luna.DEP.AST.Expr              (Expr)
import Luna.DEP.Graph.Node.StringExpr (StringExpr)



data NodeExpr = StringExpr { _strExpr :: StringExpr }
              | ASTExpr    { _expr :: Expr       }
              deriving (Show, Eq, Read)



makeLenses ''NodeExpr
