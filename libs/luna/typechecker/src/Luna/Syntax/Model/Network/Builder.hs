{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Luna.Syntax.Model.Network.Builder (module Luna.Syntax.Model.Network.Builder, module X) where

import           Luna.Syntax.Model.Graph.Builder.Class  as X
import           Luna.Syntax.Model.Network.Builder.Term as X


import           Prologue                hiding (read, Getter)
import           Control.Monad           (forM)
import           Data.Container
import           Data.Layer.Cover
import           Data.Construction
import           Data.Prop
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Luna.Syntax.Model.Graph
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Graph.Builder.Ref
import           Luna.Syntax.Model.Network.Term (Draft)
import           Luna.Evaluation.Runtime        (Static)

-------------------
-- === Utils === --
-------------------

merge :: forall n e m a.
         ( n ~ (NetLayers a :< Draft Static)
         , e ~ Link n
         , Constructor m (Ref (Node n))
         , Constructor m (Ref (Link n))
         , Getter (Ref $ Node n) (Graph n e)
         , Getter (Ref (Link n)) (Graph n e)
         , RefHandler m (Node n)
         ) => Graph n e -> m (Map (Ref $ Node n) (Ref $ Node n))
merge g = do
    let foreignNodeRefs = Ref . Ptr <$> usedIxes (g ^. nodeGraph)
        foreignEdgeRefs = Ref . Ptr <$> usedIxes (g ^. edgeGraph)

    newNodeRefs <- forM foreignNodeRefs $ construct . flip getter g

    let nodeTrans = Map.fromList $ zip foreignNodeRefs newNodeRefs
        foreignEs  = flip getter g <$> foreignEdgeRefs
        es         = foreignEs & over (mapped . source) unsafeTranslateNode
                               & over (mapped . target) unsafeTranslateNode
                   where
                   unsafeTranslateNode i = fromJust $ Map.lookup i nodeTrans

    newEdgeRefs <- forM es construct :: m [Ref e]
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
