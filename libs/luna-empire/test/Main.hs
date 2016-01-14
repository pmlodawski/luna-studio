{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prologue
import qualified Luna.Syntax.Builder.Star     as StarBuilder
import qualified Luna.Syntax.Builder.Node     as NodeBuilder
import qualified Luna.Syntax.Builder          as Builder
import           Luna.Syntax.Layer.Labeled    (label)
import           Luna.Syntax.Builder
import           Luna.Syntax.Repr.Graph
import           Luna.Diagnostic.AST          (toGraphViz, render)
import           Empire.Data.AST
import           Empire.Data.Graph
import           Empire.Data.Library
import           Empire.API.Data.Port
import           Empire.API.Data.PortRef      (InPortRef(..), OutPortRef(..))
import           Empire.API.Data.NodeMeta
import           Empire.API.Data.Node
import           Empire.Empire
import           Empire.Commands.Project
import           Empire.Commands.Library
import           Empire.Commands.AST
import           Empire.Commands.Graph        as Graph

test :: Empire AST
test = do
    (pid, _) <- createProject (Just "dupa") "/no/elo"
    (lid, _) <- createLibrary pid (Just "xd") "/xd/xd"

    n1 <- (view nodeId) <$> Graph.addNode pid lid "1"     (NodeMeta (1.0, 4.0))
    n2 <- (view nodeId) <$> Graph.addNode pid lid "2"     (NodeMeta (2.0, 3.0))
    np <- (view nodeId) <$> Graph.addNode pid lid "+"     (NodeMeta (3.0, 2.0))
    nf <- (view nodeId) <$> Graph.addNode pid lid "floor" (NodeMeta (4.0, 1.0))
    Graph.connect pid lid (OutPortRef n1 All) (InPortRef np Self)
    Graph.connect pid lid (OutPortRef np All) (InPortRef nf Self)
    Graph.connect pid lid (OutPortRef n2 All) (InPortRef np (Arg 0))

    Graph.disconnect pid lid (InPortRef nf Self)

    fstNode <- (view nodeId) <$> Graph.addNode pid lid "+"   (NodeMeta (3.14, 3.14))
    sndNode <- (view nodeId) <$> Graph.addNode pid lid "Int" (NodeMeta (4.14, 4.14))
    trdNode <- (view nodeId) <$> Graph.addNode pid lid "2"   (NodeMeta (5.14, 5.14))
    rthNode <- (view nodeId) <$> Graph.addNode pid lid "5"   (NodeMeta (6.14, 6.14))
    Graph.connect pid lid (OutPortRef rthNode All) (InPortRef fstNode (Arg 1))
    Graph.connect pid lid (OutPortRef trdNode All) (InPortRef fstNode (Arg 0))
    Graph.connect pid lid (OutPortRef sndNode All) (InPortRef fstNode Self)

    {-Graph.removeNode pid lid fstNode-}

    {-Graph.removeNode pid lid np-}

    Graph.disconnect pid lid (InPortRef fstNode Self)
    Graph.disconnect pid lid (InPortRef fstNode $ Arg 0)
    Graph.disconnect pid lid (InPortRef fstNode $ Arg 1)

    Graph.addNode pid lid "1.floor.toString" $ NodeMeta (0.0, 0.0)
    Graph.addNode pid lid "String.++ _ 1.floor.toString" $ NodeMeta (0.0, 0.0)

    code <- Graph.getCode pid lid
    putStrLn code

    graph <- Graph.getGraph pid lid
    print graph

    withLibrary pid lid (use $ body . ast)

main :: IO ()
main = do
    (graph, st) <- runEmpire def test
    case graph of
        Left err -> putStrLn err
        Right g  -> render "g" $ toGraphViz $ g
{-main = return ()-}
