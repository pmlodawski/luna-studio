{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Luna.Syntax.Model.Network.Builder (module Luna.Syntax.Model.Network.Builder, module X) where

import           Data.Graph.Builder.Class               as X
import           Luna.Syntax.Model.Network.Builder.Term as X


import           Prologue                hiding (read, Getter)
import           Control.Monad           (forM)
import           Data.Graph.Builder.Ref
import           Data.Container
import           Data.Layer.Cover
import           Data.Construction
import           Data.Prop
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Term (Draft)
import           Luna.Evaluation.Runtime        (Static)
import           Data.Graph.Backend.Vector
import           Data.Graph.Referenced


-------------------
-- === Utils === --
-------------------

merge :: forall n e m a t.
         ( n ~ (NetLayers a :< Draft Static)
         , e ~ (Link n)
         , MonadBuilder t m
         , HasRef Node n t
         , Constructor m (Ref Node n)
         , Constructor m (Ref Edge e)
         , Getter (Ref Node n) (VectorGraph n e)
         , Getter (Ref Edge e) (VectorGraph n e)
         ) => VectorGraph n e -> m (Map (Ref Node n) (Ref Node n))
merge g = do
    let foreignNodeRefs = Ref <$> usedIxes (g ^. nodeGraph)
        foreignEdgeRefs = Ref <$> usedIxes (g ^. edgeGraph)

    newNodeRefs <- forM foreignNodeRefs $ construct ∘ (flip view g ∘ ref)

    let nodeTrans = Map.fromList $ zip foreignNodeRefs newNodeRefs
        foreignEs  = flip view g ∘ ref <$> foreignEdgeRefs
        es         = foreignEs & over (mapped . source) unsafeTranslateNode
                               & over (mapped . target) unsafeTranslateNode
                   where
                   unsafeTranslateNode i = fromJust $ Map.lookup i nodeTrans

    newEdgeRefs <- forM es construct :: m [Ref Edge e]
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
