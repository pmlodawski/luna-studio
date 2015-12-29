{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prologue
import qualified Luna.Syntax.Builder.Star             as StarBuilder
import qualified Luna.Syntax.Builder.Node             as NodeBuilder
import qualified Luna.Syntax.Builder                  as Builder
import           Luna.Syntax.Layer.Labeled            (label)
import           Luna.Syntax.Builder
import           Luna.Syntax.Repr.Graph
import           Luna.Diagnostic.AST (toGraphViz, render)
import           Empire.Data.AST
import           Empire.Data.Graph
import           Empire.Data.Library
import           Empire.API.Data.Port
import           Empire.Empire
import           Empire.Commands.Project
import           Empire.Commands.Library
import           Empire.Commands.AST
import           Empire.Commands.Graph as Graph


test = do
    (pid, _) <- createProject (Just "dupa") "/no/elo"
    (lid, _) <- createLibrary pid (Just "xd") "/xd/xd"
    fstNode <- Graph.addNode pid lid "+"
    sndNode <- Graph.addNode pid lid "Int"
    trdNode <- Graph.addNode pid lid "2"
    rthNode <- Graph.addNode pid lid "5"
    Graph.connect     pid lid sndNode All fstNode Self
    -- Graph.disconnect  pid lid             fstNode Self
    Graph.connect     pid lid rthNode All fstNode (Arg 1)
    Graph.disconnect  pid lid             fstNode (Arg 1)
    Graph.connect     pid lid trdNode All fstNode (Arg 0)
    Graph.disconnect  pid lid             fstNode (Arg 0)
    withLibrary pid lid (use $ body . ast)

main = do
    (graph, s) <- runEmpire def test
    print s
    case graph of
        Left err -> putStrLn err
        Right g  -> render "g" $ toGraphViz $ g
{-main = return ()-}
