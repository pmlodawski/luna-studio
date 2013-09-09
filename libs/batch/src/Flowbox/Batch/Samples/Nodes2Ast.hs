---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Samples.Nodes2Ast where

import           Control.Applicative                       
import           System.TimeIt                             
import           Text.Show.Pretty                          

import           Flowbox.Prelude                           
import qualified Flowbox.Batch.GraphView.GraphView       as GraphView
import           Flowbox.Batch.GraphView.EdgeView          (EdgeView(..))
import qualified Flowbox.Luna.Network.Def.Definition     as Definition
import           Flowbox.Luna.Network.Def.Edge             (Edge(..))
import qualified Flowbox.Luna.Network.Def.DefManager     as DefManager
import qualified Flowbox.Luna.Network.Graph.Node         as Node
import qualified Flowbox.Luna.Passes.Graph2AST.Graph2AST as Graph2AST
import qualified Flowbox.Luna.Passes.Luna.Luna           as Luna
import qualified Flowbox.Luna.XOLD.Type.Type             as Type
import           Flowbox.Luna.XOLD.Type.Type               (Type(..))


main :: IO ()
main = timeIt main_inner *> return ()

main_inner :: IO (Either String ())
main_inner = Luna.run $ do
    let fun1_gv = GraphView.insEdges [(0, 2, EdgeView [0]    [0, 1])
                                     ,(0, 2, EdgeView [1, 3] [0, 2])
                                     ,(2, 1, EdgeView [0, 1] [0])]
                $ GraphView.insNodes [(0, Node.mkInputs)
                                     ,(1, Node.mkOutputs)
                                     ,(2, Node.mkExpr "alamakota" )] 
                                     GraphView.empty                                     
        Right fun1_graph = GraphView.toGraph fun1_gv

    let fun1_df = Definition.empty { Definition.cls = Function "fun1" (Type.Tuple []) (Type.Tuple [])
                                   , Definition.graph = fun1_graph
                                   }

        cls1_df = Definition.empty { Definition.cls = Class "Ala" ["a"] [Named "x" $ TypeName "X", Named "y" $ TypeName "Y", Named "z" $ TypeName "Int"]
                                   }

        mod1_df = Definition.empty { Definition.cls = Module "Std"  }
        mod2_df = Definition.empty { Definition.cls = Module "Test" }
        defManager = DefManager.insEdges [(2, 3, Edge)
                                         ,(3, 1, Edge)
                                         ,(3, 0, Edge)]
                   $ DefManager.insNodes [(0, fun1_df)
                                         ,(1, cls1_df) 
                                         ,(2, mod1_df) 
                                         ,(3, mod2_df)]
                                         DefManager.empty
                                   

    out_fun1 <- Graph2AST.run defManager (0, fun1_df)
    putStrLn $ ppShow out_fun1
    putStrLn "--------------------------------------------"
    out_cls1 <- Graph2AST.run defManager (1, cls1_df)
    putStrLn $ ppShow out_cls1
    putStrLn "--------------------------------------------"
    out_mod1 <- Graph2AST.run defManager (2, mod1_df)
    putStrLn $ ppShow out_mod1
