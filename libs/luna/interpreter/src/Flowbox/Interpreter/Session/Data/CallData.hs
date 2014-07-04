---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Session.Data.CallData where

import           Flowbox.Interpreter.Session.Data.CallPoint (CallPoint (CallPoint))
import           Flowbox.Interpreter.Session.Data.DefPoint  (DefPoint)
import qualified Flowbox.Interpreter.Session.Data.DefPoint  as DefPoint
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs    (Breadcrumbs)
import           Flowbox.Luna.Data.Graph.Graph              (Graph)
import           Flowbox.Luna.Data.Graph.Node               (Node)
import qualified Flowbox.Luna.Data.Graph.Node               as Node
import           Flowbox.Prelude



data CallData = CallData { _callPoint   :: CallPoint
                         , _parentBC    :: Breadcrumbs
                         , _parentGraph :: Graph
                         , _node        :: Node
                         } deriving (Show)

makeLenses (''CallData)


mk :: DefPoint -> Graph -> (Node.ID, Node) -> CallData
mk defPoint parentGraph' (nodeID, node') =
    CallData ( CallPoint (defPoint ^. DefPoint.libraryID)
                          nodeID
             )
             ( defPoint ^. DefPoint.breadcrumbs )
             parentGraph'
             node'
