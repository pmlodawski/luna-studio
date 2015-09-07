{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE CPP #-}

module AST.GraphToViz where


import           Utils.PreludePlus

import qualified Luna.Diagnostic.AST as Diag
import           Luna.Syntax.Builder.Graph hiding (get, put)
import           Luna.Syntax.Builder
import           Luna.Syntax.Layer.Typed
import           Luna.Syntax.Layer.Labeled
import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Decl

import           AST.AST

-- #define GRAPH_VIZ

#ifndef GRAPH_VIZ

graphToViz :: Show a => HomoGraph ArcPtr (Labeled a (Typed Draft)) -> IO ()
graphToViz out = putStrLn $ "GraphViz disabled\n" <> show out

#else

import           Data.GraphViz.Types.Canonical
import           Data.GraphViz.Types
import           Data.GraphViz.Attributes.Complete   hiding (Label, Int)
import qualified Data.GraphViz.Attributes.Complete   as GV
import qualified Data.GraphViz.Attributes            as GV
import           Data.GraphViz.Printing              (toDot)
import           Data.GraphViz.Commands
import qualified Data.GraphViz.Attributes.Colors     as GVC
import qualified Data.GraphViz.Attributes.Colors.X11 as GVC
import           Data.GraphViz.Printing

import           Utils.Viz

import qualified Data.Text.Lazy as Text


graphToViz :: Show a => HomoGraph ArcPtr (Labeled a (Typed Draft)) -> IO ()
graphToViz out = do
    let gv = Diag.toGraphViz out
    displayGraph $ printIt gv

#endif
