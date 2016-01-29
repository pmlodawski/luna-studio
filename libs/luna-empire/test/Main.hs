{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Empire.API.Data.DefaultValue (PortDefault (..), Value (..))
import           Empire.API.Data.Node
import           Empire.API.Data.NodeMeta
import           Empire.API.Data.Port
import           Empire.API.Data.PortRef      (InPortRef (..), OutPortRef (..))
import           Empire.Commands.AST
import           Empire.Commands.Graph        as Graph
import           Empire.Commands.Library
import           Empire.Commands.Project
import           Empire.Data.AST
import           Empire.Data.AST              (ASTNode)
import           Empire.Data.Graph
import           Empire.Data.Library
import           Empire.Empire
import           Luna.Diagnostic.AST          (LabelAttrs (..), render, toGraphViz)
import           Luna.Syntax.Builder
import qualified Luna.Syntax.Builder          as Builder
import qualified Luna.Syntax.Builder.Node     as NodeBuilder
import qualified Luna.Syntax.Builder.Star     as StarBuilder
import           Luna.Syntax.Layer.Labeled    (label)
import           Luna.Syntax.Repr.Graph
import           Prologue



test :: Empire AST
test = do
    (pid, _) <- createProject (Just "dupa") "/no/elo"
    (lid, _) <- createLibrary pid (Just "xd") "/xd/xd"

    n1 <- (view nodeId) <$> Graph.addNode pid lid "1"     (NodeMeta (1.0, 4.0))
    n2 <- (view nodeId) <$> Graph.addNode pid lid "2"     (NodeMeta (2.0, 3.0))
    np <- (view nodeId) <$> Graph.addNode pid lid "_.+ _"     (NodeMeta (3.0, 2.0))
    Graph.connect pid lid (OutPortRef n1 All) (InPortRef np Self)
    Graph.connect pid lid (OutPortRef n2 All) (InPortRef np (Arg 0))
    Graph.renameNode pid lid n1 "dupcia"

    {-fstNode <- (view nodeId) <$> Graph.addNode pid lid "+"   (NodeMeta (3.14, 3.14))-}
    {-sndNode <- (view nodeId) <$> Graph.addNode pid lid "Int" (NodeMeta (4.14, 4.14))-}
    {-trdNode <- (view nodeId) <$> Graph.addNode pid lid "2"   (NodeMeta (5.14, 5.14))-}
    {-rthNode <- (view nodeId) <$> Graph.addNode pid lid "5"   (NodeMeta (6.14, 6.14))-}
    {-Graph.connect pid lid (OutPortRef rthNode All) (InPortRef fstNode (Arg 1))-}
    {-Graph.connect pid lid (OutPortRef trdNode All) (InPortRef fstNode (Arg 0))-}
    {-Graph.connect pid lid (OutPortRef sndNode All) (InPortRef fstNode Self)-}

    {-Graph.removeNode pid lid fstNode-}

    {-Graph.removeNode pid lid np-}

    {-Graph.disconnect pid lid (InPortRef fstNode Self)-}
    {-Graph.disconnect pid lid (InPortRef fstNode $ Arg 0)-}
    {-Graph.disconnect pid lid (InPortRef fstNode $ Arg 1)-}

    {-Graph.addNode pid lid "1.floor.toString" $ NodeMeta (0.0, 0.0)-}

    rn  <- view nodeId <$> Graph.addNode pid lid "+ 7 34" (NodeMeta (3.3, 4.4))
    rn1 <- view nodeId <$> Graph.addNode pid lid "7.* 4" (NodeMeta (3.3, 4.4))

    nn <- view nodeId <$> Graph.addNode pid lid "13.+ _" def
    nn2 <- view nodeId <$> Graph.addNode pid lid "_.+ 17" def
    Graph.setDefaultValue pid lid (InPortRef nn (Arg 0)) (Constant $ IntValue 0)
    Graph.setDefaultValue pid lid (InPortRef nn2 Self) (Constant $ IntValue 14)
    putStrLn "NOW RUNNING"
    putStrLn "------------------------"

    runGraph pid lid >>= print

    putStrLn "------------------------"
    putStrLn "RUN DONE"

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
