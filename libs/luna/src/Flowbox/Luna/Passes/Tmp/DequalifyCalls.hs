---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Tmp.DequalifyCalls where

import qualified Data.List.Split                     as Split

import           Flowbox.Prelude                       
import qualified Flowbox.Luna.Network.Def.Definition as Definition
import           Flowbox.Luna.Network.Def.Definition   (Definition)
import qualified Flowbox.Luna.Network.Def.DefManager as DefManager
import           Flowbox.Luna.Network.Def.DefManager   (DefManager)
import qualified Flowbox.Luna.Network.Graph.Graph    as Graph
import           Flowbox.Luna.Network.Graph.Graph      (Graph)
import qualified Flowbox.Luna.Network.Graph.Node     as Node
import           Flowbox.Luna.Network.Graph.Node       (Node(Expr))



run :: DefManager -> DefManager
run = DefManager.nmap dequalifyDef


dequalifyDef :: Definition -> Definition
dequalifyDef def = def{ Definition.graph = dequalifyGraph $ Definition.graph def }


dequalifyGraph :: Graph -> Graph
dequalifyGraph = Graph.nmap dequalifyNode


dequalifyNode :: Node -> Node
dequalifyNode node = case node of 
    Expr {} -> node{ Node.expression = dequalifyExpression $ Node.expression node }
    _       -> node


dequalifyExpression :: String -> String
dequalifyExpression = last . (Split.splitOn ".")
