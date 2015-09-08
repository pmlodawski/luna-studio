---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Syntax.Graph.Node.Expr where

import           Flowbox.Prelude
import           Luna.Syntax.Expr                  (LExpr)
import           Luna.Syntax.Graph.Node.MultiPart  (MultiPartExpr)
import qualified Luna.Syntax.Graph.Node.MultiPart  as MultiPart
import           Luna.Syntax.Graph.Node.StringExpr (StringExpr)
import           Luna.Util.LunaShow                (lunaShow)



data NodeExpr a v = StringExpr { _strExpr :: StringExpr }
                  | MultiPart  { _multiPartExpr :: MultiPartExpr a }
                  | ASTExpr    { _expr :: LExpr a v }
                  deriving (Show, Eq, Read)


makeLenses ''NodeExpr



toStringExpr :: (Show a, Show v) => NodeExpr a v -> NodeExpr a v
toStringExpr nodeExpr = case nodeExpr of
    StringExpr {} -> nodeExpr
    MultiPart mpe -> StringExpr $ fromString $ MultiPart.getName mpe
    ASTExpr   e   -> StringExpr $ fromString $ lunaShow e
