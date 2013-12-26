---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Luna.Data.Graph.Node where

import Flowbox.Prelude



data Node = Expr     { _expr       :: String
                     , _outputName :: String
                     }
          | Inputs
          | Outputs
          deriving (Show)


makeLenses (''Node)


type ID = Int


mkExpr :: String -> String -> Node
mkExpr name outName = Expr name outName


mkInputs :: Node
mkInputs = Inputs


mkOutputs :: Node
mkOutputs = Outputs

