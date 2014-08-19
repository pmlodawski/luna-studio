---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Luna.Pass.Data.CallGraph where

import Data.Graph.Inductive               hiding (Node)
import Data.Graph.Inductive.Monad
import Data.Graph.Inductive.Monad.IOArray
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Tree          hiding (Gr)

import Flowbox.Prelude


type CallGraph = Gr () ()

type Node = Int


insert :: Node -> CallGraph -> CallGraph
insert n = insNode (n,())

connect :: (Node, Node) -> CallGraph -> CallGraph
connect (n1,n2) = insEdge (n1,n2,())

sort :: CallGraph -> [Node]
sort = topsort

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Monoid CallGraph where
    mempty = mkGraph [] []
