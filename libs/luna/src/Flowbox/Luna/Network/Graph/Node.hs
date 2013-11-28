---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Network.Graph.Node (
    Node(..),
    ID,
    mkExpr,
    mkInputs,
    mkOutputs,
) where

import           Flowbox.Prelude                    

import qualified Flowbox.Luna.Network.Flags       as Flags
import           Flowbox.Luna.Network.Flags         (Flags)
import qualified Flowbox.Luna.Network.Attributes  as Attributes
import           Flowbox.Luna.Network.Attributes    (Attributes)
import           Flowbox.Luna.Data.AST.Expr         (Expr)



data Node = Expr     { expr  :: String
                     , ast   :: Maybe Expr
                     , flags :: Flags, attributes :: Attributes }
          | Inputs   { flags :: Flags, attributes :: Attributes }
          | Outputs  { flags :: Flags, attributes :: Attributes }
          
          deriving (Show)


type ID = Int


mkExpr :: String -> Node
mkExpr name = Expr name Nothing Flags.empty Attributes.empty


mkInputs :: Node
mkInputs = Inputs Flags.empty Attributes.empty


mkOutputs :: Node
mkOutputs = Outputs Flags.empty Attributes.empty
 