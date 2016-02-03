{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# LANGUAGE CPP       #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}


module Tmp2 where

import Prologue hiding (simple, empty, Indexable, Simple, cons, lookup, index, children, Cons, Ixed, Repr, repr, minBound, maxBound, (#), assert, Index, read)

import Data.Record hiding (Layout)


import Luna.Syntax.AST.Term hiding (Lit, Val, Thunk, Expr, Draft, Target, Source, source, target)
import qualified Luna.Syntax.AST.Term as Term
import Luna.Syntax.Model.Layer.Labeled


import Data.Layer.Cover
import Data.Coat
import Data.Construction

import Control.Monad.Identity hiding (when)
import Control.Monad.State hiding (when)
import Data.Container hiding (impossible)

import           Luna.Syntax.Model.Graph.Class 
import qualified Luna.Syntax.Model.Graph.Builder as Graph
import qualified Luna.Syntax.Model.Graph.Class   as Graph

import Data.Construction

--import Control.Monad.Reader

import qualified Luna.Syntax.Model.Builder.Type as Type
import           Luna.Syntax.Model.Builder.Type (MonadTypeBuilder, TypeBuilder, TypeBuilderT)

import Luna.Syntax.Model.Builder.Self (MonadSelfBuilder, SelfBuilderT, self, setSelf, buildMe, buildAbsMe)
import qualified Luna.Syntax.Model.Builder.Self as Self

import Type.Bool

import Luna.Syntax.AST.Layout (Static, Dynamic)

import           Data.Reprx (Repr, repr)
import qualified Data.Reprx as Repr

import Data.Attribute
import Data.Index
import Control.Monad.Event

import Luna.Syntax.Model.Graph






import Luna.Syntax.Model.Layer













------------------------------------
-- === Network Implementation === --
------------------------------------

type instance Layout (Network ls) term rt = Ref $ Link (ls :< TermWrapper term rt)



-----------------------------------------
-- === Abstract building utilities === --
-----------------------------------------

class    Builder t el m  a where build :: t -> el -> m a
instance Builder t el IM a where build = impossible


-- === Utils === --

type ElemBuilder = Builder ELEMENT
buildElem :: ElemBuilder el m a => el -> m a
buildElem = build ELEMENT ; {-# INLINE buildElem #-}


-- === Instances === --

instance ( SmartCons el (Uncovered a)
         , CoverConstructor m a
         , Register ELEMENT a m
         , MonadSelfBuilder s m
         , Castable a s
         ) => Builder ELEMENT el m a where 
    -- TODO[WD]: change buildAbsMe to buildMe2
    --           and fire monad every time we construct an element, not once for the graph
    build _ el = register ELEMENT =<< buildAbsMe (constructCover $ cons el) where
    {-# INLINE build #-}



-------------------------------
-- === Node constructors === --
-------------------------------

star :: ElemBuilder Star m a => m a
star = buildElem Star

unify :: ( MonadFix m
         , ElemBuilder (Unify (Connection b u)) m u
         , Connectible a u m
         , Connectible b u m
         , Connection b u ~ Connection a u
         ) => a -> b -> m u
unify a b = mdo
    out <- buildElem $ Unify ca cb
    ca  <- connection a out
    cb  <- connection b out
    return out



--star_draft :: (ElemBuilder Star m a, Uncovered a ~ Draft Static ls) => m a
--star_draft = buildElem Star

--unify_draft :: ( MonadFix m
--               , ElemBuilder (Unify (Connection b u)) m u
--               , Connectible a u m
--               , Connectible b u m
--               , Connection b u ~ Connection a u
--               , Uncovered u ~ Draft Static ls
--               ) => a -> b -> m u
--unify_draft = unify 



--------------------------
-- === Basic layers === --
--------------------------

-- === Note === --

data Note = Note deriving (Show)
type instance AttachedData Note t = String

instance Monad m => Maker m (Tagged Note String) where make = return $ Tagged $ ""

---

data Type = Type deriving (Show)

type instance AttachedData Type t = Ref $ Link (Shelled t)

type family   Shelled a 
type instance Shelled (Raw      ls) = ls :< Raw
type instance Shelled (Draft rt ls) = ls :< Draft rt

--instance ( MonadSelfBuilder s m, MonadTypeBuilder t m, Connectible t s m, Graph.MonadBuilder n e m, x ~ s -- , ElemBuilder Star m x
--         , (Castable x s), Castable w n, (Register ELEMENT x m)
--         , a ~ Ref (Link netType)
--         , x ~ Conn_Source a
--         , w ~ Unlayered (Unlayered x)
--         , Castable (Edge netType netType) e
--         , (Register CONNECTION (Ref (Edge netType netType)) m)
--         , (MonadTypeBuilder (Ref (Node netType)) m)
--         , netType ~ NetType
--         ) => Maker m (Tagged Type a) where 
--    make = Tagged <$> do
--        s <- self
--        Type.ask >>= \case
--            Just t  -> connection t s
--            Nothing -> mdo
--                Type.set t
--                (t :: x) <- star
--                c <- connection t s
                
--                return c


instance (MonadSelfBuilder s m, Ref (Link l) ~ Connection s (Ref $ Node l), Connectible s (Ref $ Node l) m) => Maker m (Tagged Type (Ref (Link l))) where 
    make = Tagged <$> do
        s <- self
        let tgt = Ref $ Ptr 0 :: Ref $ Node l -- FIXME[WD]: Pure magic. 0 is the ID of Star
        connection tgt s



---

data Succs = Succs deriving (Show)
type instance AttachedData Succs t = [Ref $ Link (Shelled t)]

instance Monad m => Maker m (Tagged Succs [s]) where make = return $ Tagged []


-----------------------------
-- === Type Constraint === --
-----------------------------

-- === Definitions === ---

data SuccRegister = SuccRegister deriving (Show)
instance ( Monad  m
         , Reader m (Edge src tgt)
         , Reader m (Node src)
         , Writer m (Node src)
         , Show src
         , Attr Succs src ~ [Ref (Edge src tgt)]
         , HasAttr Succs src
         ) => Handler t SuccRegister m (Ref (Edge src tgt)) where 
    handler e = do
        ve <- lift $ read e -- FIXME[WD]: remove the lift (it could be handy to disable the magic trans-instance in Graph.hs)
        lift $ with (ve ^. source) $ attr Succs %~ (e:)
    {-# INLINE handler #-}


-- === Utils === ---

registerSuccs :: t -> Listener t SuccRegister m a -> m a
registerSuccs _ = unwrap'


------------------------------------------------------------

-- FIXME[WD]: poprawic typ oraz `WithElement_` (!)
inputs :: forall x ast rt ls. 
      ( x ~ Ref (Link (ls :< ast rt))
      , (MapTryingElemList_ (Props Variant (RecordOf (RecordOf (ast rt ls)))) (TFoldable (Ref (Link (ls :< ast rt)))) (ast rt ls))
      ) => ast rt ls -> [x]
inputs a = withElement_ (p :: P (TFoldable x)) (foldrT (:) []) a


-------------------------------------------------------------------------------------------------------------------------------------------------
-- TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST --
-------------------------------------------------------------------------------------------------------------------------------------------------

type NetLayers = '[Type, Succs]
type NetType   = NetLayers :< Draft Static

type NetGraph = Graph (NetLayers :< Raw) (Link (NetLayers :< Raw))

buildNetwork  = runIdentity ∘ buildNetworkM
buildNetworkM = rebuildNetworkM def
rebuildNetworkM (net :: NetGraph) = flip Self.evalT (undefined ::        Ref $ Node NetType)
                                  ∘ flip Type.evalT (Nothing   :: Maybe (Ref $ Node NetType))
                                  ∘ constrainTypeM1 CONNECTION (Proxy :: Proxy $ Ref c)
                                  ∘ constrainTypeEq ELEMENT    (Proxy :: Proxy $ Ref $ Node NetType)
                                  -- ∘ constrainTypeM3 ELEMENT    (Proxy :: Proxy $ Ref $ Node (NetLayers :< n))
                                  ∘ flip Graph.runT net
                                  ∘ registerSuccs   CONNECTION
{-# INLINE   buildNetworkM #-}
{-# INLINE rebuildNetworkM #-}

rebuildNetworkM_NoSuccessors (net :: NetGraph) 
    = flip Self.evalT (undefined ::        Ref $ Node NetType)
    ∘ flip Type.evalT (Nothing   :: Maybe (Ref $ Node NetType))
    ∘ constrainTypeM1 CONNECTION (Proxy :: Proxy $ Ref c)
    ∘ constrainTypeEq ELEMENT    (Proxy :: Proxy $ Ref $ Node NetType)
    ∘ flip Graph.runT net




