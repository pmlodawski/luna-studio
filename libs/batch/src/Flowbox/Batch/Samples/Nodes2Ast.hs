---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Flowbox.Batch.Samples.Nodes2Ast where

import           Text.Show.Pretty                          
import qualified Flowbox.Batch.Batch                     as Batch
import           Flowbox.Batch.Batch                       (Batch(..))
import qualified Flowbox.Batch.GraphView.GraphView       as GraphView
import qualified Flowbox.Batch.GraphView.EdgeView        as EdgeView
import           Flowbox.Batch.GraphView.EdgeView          (EdgeView(..))
import qualified Flowbox.Batch.Handlers.Defs             as BatchD
import qualified Flowbox.Batch.Handlers.Graph            as BatchG
import qualified Flowbox.Batch.Handlers.Libs             as BatchL
import qualified Flowbox.Batch.Handlers.Projects         as BatchP
import qualified Flowbox.Batch.Project.Project           as Project
import           Flowbox.Batch.Project.Project             (Project(..))
import           Flowbox.Control.Error                     
import qualified Flowbox.Luna.Lib.LibManager             as LibManager
import           Flowbox.Luna.Lib.LibManager               (LibManager)
import qualified Flowbox.Luna.Lib.Library                as Library
import           Flowbox.Luna.Lib.Library                  (Library(..))
import qualified Flowbox.Luna.Network.Attributes         as Attributes
import qualified Flowbox.Luna.Network.Def.Definition     as Definition
import           Flowbox.Luna.Network.Def.Definition       (Definition(..))
import qualified Flowbox.Luna.Network.Def.DefManager     as DefManager
import           Flowbox.Luna.Network.Def.DefManager       (DefManager)
import qualified Flowbox.Luna.Network.Graph.DefaultValue as DefaultValue
import qualified Flowbox.Luna.Network.Graph.Edge         as Edge
import           Flowbox.Luna.Network.Graph.Edge           (Edge(..))
import qualified Flowbox.Luna.Network.Graph.Graph        as Graph
import qualified Flowbox.Luna.Network.Graph.Node         as Node
import           Flowbox.Luna.Network.Graph.Node           (Node(..))
import qualified Flowbox.Luna.Type.Type                  as Type
import           Flowbox.Luna.Type.Type                    (Type(..))
import qualified Flowbox.System.UniPath                  as UniPath
import           Flowbox.System.UniPath                    (UniPath)
import qualified Flowbox.Batch.Tools.Definition2AST      as Definition2AST

main :: IO ()
main = eRunScript $ do
    let fun1_gv = GraphView.insEdges [(0, 2, EdgeView [0]    [0])
                                     ,(2, 1, EdgeView [0, 1] [0])]
                $ GraphView.insNodes [(0, Node.mkInputs)
                                     ,(1, Node.mkOutputs)
                                     ,(2, Node.mkExpr "alamakota" )] 
                                     GraphView.empty                                     
    fun1_graph <- tryRight $ GraphView.toGraph fun1_gv

    let fun1_df = Definition.empty { Definition.cls = Function "fun1" (Type.Tuple []) (Type.Tuple [])
                                   , Definition.graph = fun1_graph
                                   }

        cls1_df = Definition.empty { Definition.cls = Class "Ala" ["a"] [Named "x" $ Type "X", Named "y" $ Type "Y", Named "z" $Type "Int"]
                                   }

        defManager = DefManager.insNodes [(0, fun1_df)
                                         ,(1, cls1_df)] 
                                         DefManager.empty
                                   

    scriptIO $ do 
                  --print fun1_df
                  putStrLn $ ppShow $ Definition2AST.def2AST defManager (0, fun1_df)
                  
                  putStrLn "--------------------------------------------"

                  --print cls1_df
                  putStrLn $ ppShow $ Definition2AST.def2AST defManager (1, cls1_df)
