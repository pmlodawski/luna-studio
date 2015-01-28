---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Syntax.Graph.Node where

import           Flowbox.Prelude
import           Luna.Syntax.Graph.Node.Expr       (NodeExpr)
import qualified Luna.Syntax.Graph.Node.Expr       as NodeExpr
import           Luna.Syntax.Graph.Node.Position   (Position)
import qualified Luna.Syntax.Graph.Node.StringExpr as StringExpr
import           Luna.Syntax.Name                  (VNameP)



data Node a e = Expr     { _expr :: NodeExpr a e, _outputName :: VNameP, _pos :: Position }
              | Inputs   {                                               _pos :: Position }
              | Outputs  {                                               _pos :: Position }
              deriving (Show, Eq)


makeLenses ''Node


type ID = Int



position' :: (ID, Node a e) -> Position
position' = view pos . snd


isInputs :: Node a e -> Bool
isInputs (Inputs {}) = True
isInputs _           = False


isOutputs :: Node a e -> Bool
isOutputs (Outputs {}) = True
isOutputs _            = False


isExpr :: Node a e -> Bool
isExpr (Expr {}) = True
isExpr _         = False


exprStr :: Node a e -> Maybe String
exprStr (Expr (NodeExpr.StringExpr strExpr)  _ _) = Just $ StringExpr.toString strExpr
exprStr _                                         = Nothing
