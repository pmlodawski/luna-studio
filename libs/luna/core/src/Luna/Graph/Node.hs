---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Graph.Node where

import Flowbox.Prelude



data Node = Expr     { _expr :: String, _outputName :: String, _pos :: Position }
          | Inputs   {                                         _pos :: Position }
          | Outputs  {                                         _pos :: Position }
          deriving (Show, Eq)


type Position = (Float, Float)

makeLenses ''Node


type ID = Int



position' :: (ID, Node) -> (Float, Float)
position' = view pos . snd


isInputs :: Node -> Bool
isInputs (Inputs {}) = True
isInputs _           = False


isOutputs :: Node -> Bool
isOutputs (Outputs {}) = True
isOutputs _            = False


isExpr :: Node -> Bool
isExpr (Expr {}) = True
isExpr _         = False
