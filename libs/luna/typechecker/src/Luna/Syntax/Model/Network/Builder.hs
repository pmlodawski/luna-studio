{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Luna.Syntax.Model.Network.Builder (module Luna.Syntax.Model.Network.Builder, module X) where

import           Data.Graph.Builder.Class               as X
import           Luna.Syntax.Model.Network.Builder.Term as X


import           Prologue                hiding (read, Getter)
import           Control.Monad           (forM)
import           Data.Graph.Builder
import           Data.Graph.Backend.VectorGraph
import           Data.Container
import           Data.Layer.Cover
import           Data.Construction
import           Data.Prop
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Term (Draft)
import           Luna.Evaluation.Runtime        (Static)


-------------------
-- === Utils === --
-------------------

merge :: ( node  ~ (NetLayers a :< Draft Static)
         , edge  ~ (Link node)
         , graph ~ Hetero (VectorGraph n e)
         , BiCastable e edge
         , BiCastable n node
         , MonadBuilder graph m
         , Referred Node n graph
         , Constructor m (Ref Node node)
         , Constructor m (Ref Edge edge)
         ) => graph -> m (Map (Ref Node node) (Ref Node node))
merge g = do
    let foreignNodeRefs = Ref <$> usedIxes (g ^. wrapped . nodeGraph)
        foreignEdgeRefs = Ref <$> usedIxes (g ^. wrapped . edgeGraph)

    newNodeRefs <- forM foreignNodeRefs $ construct . (flip view g . focus)

    let nodeTrans = Map.fromList $ zip foreignNodeRefs newNodeRefs
        foreignEs  = flip view g ∘ focus <$> foreignEdgeRefs
        es         = foreignEs & over (mapped . source) unsafeTranslateNode
                               & over (mapped . target) unsafeTranslateNode
                   where
                   unsafeTranslateNode i = fromJust $ Map.lookup i nodeTrans

    newEdgeRefs <- forM es construct
    let edgeTrans = Map.fromList $ zip foreignEdgeRefs newEdgeRefs

    forM newNodeRefs $ \ref -> do
        node <- read ref
        let nodeWithFixedEdges = node & over covered (fmapInputs unsafeTranslateEdge)
                                      & over (prop Succs . mapped) unsafeTranslateEdge
                                      & over (prop Type)           unsafeTranslateEdge
                where
                unsafeTranslateEdge i = fromJust $ Map.lookup i edgeTrans
        write ref nodeWithFixedEdges

    return nodeTrans
