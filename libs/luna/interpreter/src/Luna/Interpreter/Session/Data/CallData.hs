---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Data.CallData where

import           Flowbox.Prelude
import qualified Luna.AST.Common                         as AST
import           Luna.AST.Control.Crumb                  (Breadcrumbs)
import           Luna.Graph.Graph                        (Graph)
import           Luna.Graph.Node                         (Node)
import qualified Luna.Graph.Node                         as Node
import           Luna.Interpreter.Session.Data.CallPoint (CallPoint (CallPoint))
import           Luna.Interpreter.Session.Data.DefPoint  (DefPoint)
import qualified Luna.Interpreter.Session.Data.DefPoint  as DefPoint



data CallData = CallData { _callPoint   :: CallPoint
                         , _parentBC    :: Breadcrumbs
                         , _parentDefID :: AST.ID
                         , _parentGraph :: Graph
                         , _node        :: Node
                         } deriving (Show, Eq)

makeLenses (''CallData)


mk :: DefPoint -> AST.ID -> Graph -> (Node.ID, Node) -> CallData
mk defPoint parentDefID' parentGraph' (nodeID, node') =
    CallData ( CallPoint (defPoint ^. DefPoint.libraryID)
                          nodeID
             )
             ( defPoint ^. DefPoint.breadcrumbs )
             parentDefID'
             parentGraph'
             node'
