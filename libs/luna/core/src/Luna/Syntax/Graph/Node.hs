---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Syntax.Graph.Node where

import           Flowbox.Prelude
import           Luna.Syntax.Graph.DefaultsMap   (DefaultsMap)
import           Luna.Syntax.Graph.Node.Expr     (NodeExpr)
import qualified Luna.Syntax.Graph.Node.Expr     as NodeExpr
import           Luna.Syntax.Graph.Node.Position (Position)
import           Luna.Syntax.Pat                 (LPat)



data Node a v = Expr    { _expr      :: NodeExpr a v
                        , _outputPat :: Maybe (LPat a)
                        , _defaults  :: DefaultsMap a v
                        , _pos       :: Position
                        , _groupInfo :: [a]
                        , _generated :: Bool
                        }
              | Inputs  { _pos :: Position
                        }
              | Outputs { _defaults :: DefaultsMap a v
                        , _pos      :: Position
                        }
              deriving (Show, Eq)


makeLenses ''Node


type ID = Int


inputsID :: ID
inputsID = -2


outputID :: ID
outputID = -1


mkInputs :: Node a v
mkInputs = Inputs def

mkOutputs :: Node a v
mkOutputs = Outputs def def


position' :: (ID, Node a v) -> Position
position' = view pos . snd


isInputs :: Node a v -> Bool
isInputs (Inputs {}) = True
isInputs _           = False


isOutputs :: Node a v -> Bool
isOutputs (Outputs {}) = True
isOutputs _            = False


isExpr :: Node a v -> Bool
isExpr (Expr {}) = True
isExpr _         = False


exprStr :: Node a v -> Maybe String
exprStr (Expr (NodeExpr.StringExpr strExpr) _ _ _ _ _) = Just $ toString strExpr
exprStr _                                              = Nothing


getOutputPat :: Node a v -> Maybe (LPat a)
getOutputPat (Expr { _outputPat = pat }) = pat
getOutputPat _                           = Nothing


toStringNode :: (Show a, Show v) => Node a v -> Node a v
toStringNode = expr %~ NodeExpr.toStringExpr
--insertDefault :: PortDescriptor -> NodeExpr a v -> Node a v -> Node a v
--insertDefault pd ne = defaults %~ DefaultsMap.insert pd ne
