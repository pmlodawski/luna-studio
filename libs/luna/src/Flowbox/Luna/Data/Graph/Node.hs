---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Graph.Node (
    Node(..),
    ID,
    mkExpr,
    mkInputs,
    mkOutputs,
) where

import Flowbox.Prelude

import           Flowbox.Luna.Data.AST.Expr         (Expr)
import           Flowbox.Luna.Data.Graph.Properties (Properties)
import qualified Flowbox.Luna.Data.Graph.Properties as Properties



data Node = Expr     { expr       :: String
                     , ast        :: Maybe Expr
                     , properties :: Properties }
          | Inputs   { properties :: Properties }
          | Outputs  { properties :: Properties }

          deriving (Show)


type ID = Int


mkExpr :: String -> Node
mkExpr name = Expr name Nothing Properties.empty


mkInputs :: Node
mkInputs = Inputs Properties.empty


mkOutputs :: Node
mkOutputs = Outputs Properties.empty

