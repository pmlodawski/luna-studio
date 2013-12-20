---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Luna.Data.Graph.Node where

import           Flowbox.Luna.Data.Graph.Properties (Properties)
import qualified Flowbox.Luna.Data.Graph.Properties as Properties
import           Flowbox.Prelude



data Node = Expr     { _expr       :: String
                     , _outputName :: String
                     , _properties :: Properties }
          | Inputs   { _properties :: Properties }
          | Outputs  { _properties :: Properties }
          deriving (Show)


makeLenses (''Node)


type ID = Int


mkExpr :: String -> String -> Node
mkExpr name outName = Expr name outName Properties.empty


mkInputs :: Node
mkInputs = Inputs Properties.empty


mkOutputs :: Node
mkOutputs = Outputs Properties.empty

