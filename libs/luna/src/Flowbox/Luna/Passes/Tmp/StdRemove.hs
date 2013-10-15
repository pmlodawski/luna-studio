---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Tmp.StdRemove where

import qualified Data.List                           as List

import           Flowbox.Prelude                       
import qualified Flowbox.Luna.Network.Def.Definition as Definition
import           Flowbox.Luna.Network.Def.Definition   (Definition)
import qualified Flowbox.Luna.Network.Def.DefManager as DefManager
import           Flowbox.Luna.Network.Def.DefManager   (DefManager)
import qualified Flowbox.Luna.Network.Graph.Graph    as Graph
import           Flowbox.Luna.Network.Graph.Graph      (Graph)
import qualified Flowbox.Luna.Network.Graph.Node     as Node
import           Flowbox.Luna.Network.Graph.Node       (Node(Expr))


stdPrexix :: String
stdPrexix = "Std."


run :: DefManager -> DefManager
run = DefManager.nmap removeFromDef


removeFromDef :: Definition -> Definition
removeFromDef def = def{ Definition.graph = removeFromGraph $ Definition.graph def }


removeFromGraph :: Graph -> Graph
removeFromGraph = Graph.nmap removeFromNode


removeFromNode :: Node -> Node
removeFromNode node = case node of 
    Expr {} -> node{ Node.expression = removeFromExpression $ Node.expression node }
    _       -> node


removeFromExpression :: String -> String
removeFromExpression expr = case List.stripPrefix stdPrexix expr of 
    Just stripped -> stripped
    Nothing       -> expr
