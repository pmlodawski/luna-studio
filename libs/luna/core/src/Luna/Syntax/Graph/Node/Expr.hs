---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Syntax.Graph.Node.Expr where

import Flowbox.Prelude
import Luna.Syntax.Expr                  (LExpr)
import Luna.Syntax.Graph.Node.StringExpr (StringExpr)



data NodeExpr a v = StringExpr { _strExpr :: StringExpr }
                  | ASTExpr    { _expr :: LExpr a v }
                  deriving (Show, Eq, Read)


makeLenses ''NodeExpr
