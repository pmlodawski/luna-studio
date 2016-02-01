{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Tmp2 where

import Prologue hiding (simple, empty, Indexable, Simple, cons, lookup, index, children, Cons, Ixed, Repr, repr, minBound, maxBound, (#), assert, Index, read)

import Data.Record hiding (Layout)


import Luna.Syntax.AST.Term2 hiding (Lit, Val, Thunk, Expr, Draft, Target, Source, source, target)
import qualified Luna.Syntax.AST.Term2 as Term
import Luna.Syntax.Model.Layer.Labeled


import Data.Layer.Cover
import Data.Coat
import Data.Construction

import Control.Monad.Identity hiding (when)
import Control.Monad.State hiding (when)
import Data.Container hiding (impossible)

import           Luna.Syntax.Model.Graph (Graph, GraphBuilder, MonadGraphBuilder, nodes, edges)
import qualified Luna.Syntax.Model.Graph as Graph

import Data.Construction

--import Control.Monad.Reader

import qualified Luna.Syntax.Model.Builder.Type as Type
import           Luna.Syntax.Model.Builder.Type (MonadTypeBuilder, TypeBuilder, TypeBuilderT)

import Luna.Syntax.Model.Builder.Self (MonadSelfBuilder, SelfBuilderT, self, setSelf, buildMe, buildAbsMe)
import qualified Luna.Syntax.Model.Builder.Self as Self

import Type.Bool

import Luna.Syntax.AST.Layout (Static, Dynamic)



newtype Tagged t a = Tagged a deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

makeWrapped ''Tagged

instance Castable a a' => Castable (Tagged t a) (Tagged t a') where cast = wrapped %~ cast ; {-# INLINE cast #-}

----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------



class HasIdx a where idx :: Lens' a (Index a)


----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------


------------------------
-- === Attributes === --
------------------------

type family Attr a t
class HasAttr a t where attr :: a -> Lens' t (Attr a t)


----------------------
-- === Listener === --
----------------------

-- === Definitions === --

class Handler    t cfg m a where handler :: a -> Listener t cfg m ()
newtype Listener t cfg m a = Listener (m a) deriving (Show)
makeWrapped ''Listener


-- === Instances === --

instance Functor  m    => Functor     (Listener t cfg m) where fmap     f = wrapped %~ fmap f                    ; {-# INLINE fmap   #-}
instance Monad    m    => Monad       (Listener t cfg m) where (>>=) tc f = wrap' $ unwrap' tc >>= unwrap' <$> f ; {-# INLINE (>>=)  #-}
instance MonadIO  m    => MonadIO     (Listener t cfg m) where liftIO     = wrap' ∘ liftIO                       ; {-# INLINE liftIO #-}
instance MonadFix m    => MonadFix    (Listener t cfg m) where mfix     f = wrap' $ mfix $ unwrap' <$> f         ; {-# INLINE mfix   #-}
instance                  MonadTrans  (Listener t cfg)   where lift       = wrap'                                ; {-# INLINE lift   #-}
instance Applicative m => Applicative (Listener t cfg m) where pure       = wrap' ∘ pure                         ; {-# INLINE pure   #-}
                                                               (<*>)  f a = wrap' $ unwrap' f <*> unwrap' a      ; {-# INLINE (<*>)  #-}

-- Registration time type constraint

instance {-# OVERLAPPABLE #-} (Monad m, Register t a m)                    => Register t a (Listener t' cfg m) where register_     = lift ∘∘ register_
instance {-# OVERLAPPABLE #-} (Monad m, Register t a m, Handler t cfg m a) => Register t a (Listener t  cfg m) where register_ t a = handler a *> (lift $ register_ t a)







-----------------------------
-- === Type Constraint === --
-----------------------------

-- === Definitions === ---

data TypeConstraint (ctx :: * -> * -> Constraint) tp
instance (ctx a tp, Monad m) => Handler t (TypeConstraint ctx tp) m a where handler _ = return () ; {-# INLINE handler #-}


-- === Constraint rules === ---

class Equality_Full a b
instance a ~ b => Equality_Full a b

class Equality_M1 a b
instance (a ~ ma pa, b ~ mb pb, ma ~ mb) => Equality_M1 a b

class Equality_M2 a b
instance (a ~ m1a (m2a pa), b ~ m1b (m2b pb), m1a ~ m1b, m2a ~ m2b) => Equality_M2 a b 

class Equality_M3 a b
instance (a ~ m1a (m2a (m3a pa)), b ~ m1b (m2b (m3b pb)), m1a ~ m1b, m2a ~ m2b, m3a ~ (m3b :: ([*] -> *) -> *)) => Equality_M3 a b 
-- FIXME[WD]: remove the kind constraint above


-- === Utils === ---

constrainType :: Proxy ctx -> t -> Proxy tp -> Listener t (TypeConstraint ctx tp) m a -> m a
constrainType _ _ _ = unwrap'

constrainTypeEq :: t -> Proxy tp -> Listener t (TypeConstraint Equality_Full tp) m a -> m a
constrainTypeM1 :: t -> Proxy tp -> Listener t (TypeConstraint Equality_M1   tp) m a -> m a
constrainTypeM2 :: t -> Proxy tp -> Listener t (TypeConstraint Equality_M2   tp) m a -> m a
constrainTypeM3 :: t -> Proxy tp -> Listener t (TypeConstraint Equality_M3   tp) m a -> m a
constrainTypeEq = constrainType (p :: P Equality_Full)
constrainTypeM1 = constrainType (p :: P Equality_M1)
constrainTypeM2 = constrainType (p :: P Equality_M2)
constrainTypeM3 = constrainType (p :: P Equality_M3)





----------------------
-- === Register === --
----------------------
-- | The `register` function can be used to indicate that a particular element is "done".
--   It does not provide any general special meaning. In general, this information can be lost when not used explicitly.
--   For a specific usage look at the `Network` builder, where `register` is used to add type constrains on graph nodes and edges.
--   The `t` parameter is the type of registration, like `Node` or `Edge`. Please keep in mind, that `Node` indicates a "kind" of a structure.
--   It does not equals a graph-like node - it can be a "node" in flat AST representation, like just an ordinary term.


data ELEMENT    = ELEMENT    deriving (Show)
data CONNECTION = CONNECTION deriving (Show)

class Monad m => Register t a m where 
    register_ :: t -> a -> m ()


-- === Utils === --

registerM :: Register t a m => t -> m a -> m a
registerM t ma = do
    a <- ma
    register_ t a
    return a
{-# INLINE registerM #-}

register :: Register t a m => t -> a -> m a
register t a = a <$ register_ t a ; {-# INLINE register #-}


-- === Instances === --

instance Register t a m => Register t a (Graph.GraphBuilderT n e m) where register_     = lift ∘∘ register_ ; {-# INLINE register_ #-}
instance Register t a m => Register t a (StateT                s m) where register_     = lift ∘∘ register_ ; {-# INLINE register_ #-}
instance Register t a m => Register t a (TypeBuilderT          s m) where register_     = lift ∘∘ register_ ; {-# INLINE register_ #-}
instance Register t a m => Register t a (SelfBuilderT          s m) where register_     = lift ∘∘ register_ ; {-# INLINE register_ #-}
instance                   Register t a IO                          where register_ _ _ = return ()         ; {-# INLINE register_ #-}
instance                   Register t a Identity                    where register_ _ _ = return ()         ; {-# INLINE register_ #-}




------------------------------
-- === Graph references === --
------------------------------

-- === Definitions === --

data Ptr i = Ptr i         deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data Ref a = Ref (Ptr Int) deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

makeWrapped ''Ptr
makeWrapped ''Ref

type family Target a

class HasPtr   a where ptr   :: Lens' a (Ptr (Index  a))
class HasRef   a where ref   :: Lens' a (Ref (Target a))

class Monad m => Reader m a where read  :: Ref a -> m a
class Monad m => Writer m a where write :: Ref a -> a -> m ()

type Modifier m a = (Reader m a, Writer m a)


-- === Utils === --

withM :: Modifier m a => Ref a -> (a -> m a) -> m ()
withM ref f = read ref >>= f >>= write ref

with :: Modifier m a => Ref a -> (a -> a) -> m ()
with ref = withM ref ∘ (return <$>)


-- === Instances === --

-- Ptr primitive instances

type instance Index  (Ptr i) = i
instance      HasIdx (Ptr i) where idx = wrapped'
instance      HasPtr (Ptr i) where ptr = id

-- Ref primitive instances

type instance Unlayered  (Ref a) = a
type instance Destructed (Ref a) = a
type instance Target     (Ref a) = a
type instance Index      (Ref a) = Index (Unwrapped (Ref a))
instance      HasRef     (Ref a) where ref = id
instance      HasIdx     (Ref a) where idx = ptr ∘ idx
instance      HasPtr     (Ref a) where ptr = wrapped'

-- Ref construction

instance (MonadGraphBuilder n e m, Castable a n) => Constructor m (Ref (Node a)) where 
    construct n = Ref ∘ Ptr <$> Graph.modify (nodes $ swap ∘ ixed add (cast ast)) where
        ast = unwrap' n :: a

instance (MonadGraphBuilder n e m, Castable (Edge src tgt) e) => Constructor m (Ref (Edge src tgt)) where 
    construct e = Ref ∘ Ptr <$> Graph.modify (edges $ swap ∘ ixed add (cast e)) where

instance Constructor m (Ref ref) => LayerConstructor m (Ref ref) where
    constructLayer = construct ; {-# INLINE constructLayer #-}

-- Ref reading / writing

instance (MonadGraphBuilder n e m, Castable n a) => Reader m (Node a) where
    read ref = Node ∘ cast ∘ index_ (ref ^. idx) ∘ view nodes <$> Graph.get ; {-# INLINE read #-}

instance (MonadGraphBuilder n e m, Castable a n) => Writer m (Node a) where
    write ref val = Graph.modify_ $ nodes %~ unchecked inplace insert_ (ref ^. idx) (cast $ unwrap' val) ; {-# INLINE write #-}

instance (MonadGraphBuilder n e m, Castable e (Edge src tgt)) => Reader m (Edge src tgt) where
    read ref = cast ∘ index_ (ref ^. idx) ∘ view edges <$> Graph.get ; {-# INLINE read #-}

-- Conversions

instance Castable a a' => Castable (Ref a) (Ref a') where cast = rewrap ; {-# INLINE cast #-}



--------------------------------
-- === Network Structures === --
--------------------------------

data Network (ls :: [*]) = Network

newtype Node       a = Node a                   deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data    Edge src tgt = Edge (Ref src) (Ref tgt) deriving (Show, Eq, Ord)
type    Link       a = Edge a a

type family Connection  src dst
type family Conn_Source a -- TODO[WD]: change name after refactoring
type family Conn_Target a -- TODO[WD]: change name after refactoring

class ( src ~ Conn_Source (Connection src tgt)
      , tgt ~ Conn_Target (Connection src tgt)
      ) => Connectible src tgt m where connection :: src -> tgt -> m (Connection src tgt)


-- === Utils === --

edge :: Ref (Node src) -> Ref (Node tgt) -> Edge src tgt
edge src tgt = Edge (rewrap src) (rewrap tgt)

source :: Edge src tgt -> Ref (Node src)
source (Edge src _) = rewrap src

target :: Edge src tgt -> Ref (Node tgt)
target (Edge _ tgt) = rewrap tgt

follow :: (Reader m (Edge src tgt), Functor m) => Ref (Edge src tgt) -> m (Ref $ Node tgt)
follow ptr = target <$> read ptr


-- === Instances === --

-- Primitive

type instance Conn_Source (Ref a) = Ref (Conn_Source a)
type instance Conn_Target (Ref a) = Ref (Conn_Target a)
type instance Conn_Source (Edge src tgt) = Node src
type instance Conn_Target (Edge src tgt) = Node tgt

-- Wrappers

makeWrapped ''Node
type instance Unlayered (Node a) = (Unwrapped (Node a))
instance      Layered   (Node a)

-- Connecting

type instance Connection (Ref  a) (Ref  b) = Ref (Connection a b)
type instance Connection (Node a) (Node b) = Edge a b

instance (LayerConstructor m c, Register CONNECTION c m, Unlayered c ~ Edge src tgt, c ~ Connection (Ref (Node src)) (Ref (Node tgt))) 
      => Connectible (Ref (Node src)) (Ref (Node tgt)) m where
         connection src tgt = register CONNECTION =<< constructLayer (edge src tgt)

-- Construction

instance Monad m => LayerConstructor m (Node a) where
    constructLayer = return ∘ Node ; {-# INLINE constructLayer #-}

-- Conversions

instance (Castable (Ref src) (Ref src'), Castable (Ref tgt) (Ref tgt')) => Castable (Edge src tgt) (Edge src' tgt') where 
    cast (Edge src tgt) = Edge (cast src) (cast tgt) ; {-# INLINE cast #-}

instance Castable a a' => Castable (Node a) (Node a') where
    cast = wrapped %~ cast

-- Attributes

type instance Attr a (Node t) = Attr a t
instance HasAttr a t => HasAttr a (Node t) where attr = wrapped' ∘∘ attr




---------------------------
-- === Network Terms === --
---------------------------

-- === Definitions === --

type family TermWrapper (a :: *) :: * -> [*] -> *

data    Raw      (ls :: [*]) = Raw Data                                deriving (Show)

newtype Lit   rt (ls :: [*]) = Lit   (Term (Network ls) Term.Lit   rt) deriving (Show)
newtype Val   rt (ls :: [*]) = Val   (Term (Network ls) Term.Val   rt) deriving (Show)
newtype Thunk rt (ls :: [*]) = Thunk (Term (Network ls) Term.Thunk rt) deriving (Show)
newtype Expr  rt (ls :: [*]) = Expr  (Term (Network ls) Term.Expr  rt) deriving (Show)
newtype Draft rt (ls :: [*]) = Draft (Term (Network ls) Term.Draft rt) deriving (Show)


-- === Instances === --

-- Wrappers

makeWrapped ''Raw

makeWrapped ''Lit
makeWrapped ''Val
makeWrapped ''Thunk
makeWrapped ''Expr
makeWrapped ''Draft

-- Term bindings

type instance TermWrapper Term.Lit   = Lit
type instance TermWrapper Term.Val   = Val
type instance TermWrapper Term.Thunk = Thunk
type instance TermWrapper Term.Expr  = Expr
type instance TermWrapper Term.Draft = Draft

-- Records

type instance RecordOf (Lit   rt ls) = RecordOf (Unwrapped (Lit   rt ls))
type instance RecordOf (Val   rt ls) = RecordOf (Unwrapped (Val   rt ls))
type instance RecordOf (Thunk rt ls) = RecordOf (Unwrapped (Thunk rt ls))
type instance RecordOf (Expr  rt ls) = RecordOf (Unwrapped (Expr  rt ls))
type instance RecordOf (Draft rt ls) = RecordOf (Unwrapped (Draft rt ls))

instance IsRecord (Lit   rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Val   rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Thunk rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Expr  rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Draft rt ls) where asRecord = wrapped' ∘ asRecord

-- Conversions

instance Castable (Raw      ls) (Raw      ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Lit   rt ls) (Lit   rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Val   rt ls) (Val   rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Thunk rt ls) (Thunk rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Expr  rt ls) (Expr  rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Draft rt ls) (Draft rt ls) where cast = id ; {-# INLINE cast #-}

instance Castable (Lit   rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Val   rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Thunk rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Expr  rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Draft rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}

instance Castable (Raw ls) (Lit   rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Val   rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Thunk rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Expr  rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Draft rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}



--------------------
-- === Layers === --
--------------------

-- === Definition === --

data Layer t a = Layer (LayerData (Layer t a)) a
type family AttachedData d a


-- === Utils === --

type family LayerData l where LayerData (Layer t a) = Tagged t (AttachedData t (Uncovered a))


-- === Instances === --

deriving instance (Show (AttachedData t (Uncovered a)), Show a) => Show (Layer t a)

type instance Unlayered (Layer t a) = a
instance      Layered (Layer t a) where
    layered = lens (\(Layer _ a) -> a) (\(Layer d _) a -> Layer d a) ; {-# INLINE layered #-}

instance (Maker m (LayerData (Layer t a)), Functor m)
      => LayerConstructor m (Layer t a) where
    constructLayer a = flip Layer a <$> make ; {-# INLINE constructLayer #-}

instance (Castable a a', Castable (LayerData (Layer t a)) (LayerData (Layer t' a')))
      => Castable (Layer t a) (Layer t' a') where
    cast (Layer d a) = Layer (cast d) (cast a) ; {-# INLINE cast #-}

-- Attributes

type instance Attr a (Layer t l) = If (a == t) (AttachedData t (Uncovered l)) (Attr a l)
instance {-# OVERLAPPABLE #-} (Attr a (Layer t l) ~ Attr a (Unlayered (Layer t l)), HasAttr a l) 
                           => HasAttr a (Layer t l) where attr   = layered ∘∘ attr                                                   ; {-# INLINE attr #-}
instance {-# OVERLAPPABLE #-} HasAttr a (Layer a l) where attr _ = lens (\(Layer d _) -> d) (\(Layer _ a) d -> Layer d a) ∘ wrapped' ; {-# INLINE attr #-}


--------------------
-- === Shell === ---
--------------------

data (layers :: [*]) :< (a :: [*] -> *) = Shell (ShellStrcture layers (a layers))

type family ShellStrcture ls a where 
    ShellStrcture '[]       a = Cover a
    ShellStrcture (l ': ls) a = Layer l (ShellStrcture ls a)


-- === Instances === --

deriving instance Show (Unwrapped (ls :< a)) => Show (ls :< a)

makeWrapped ''(:<)
type instance Unlayered (ls :< a) = Unwrapped (ls :< a)
instance      Layered   (ls :< a)

instance Monad m => LayerConstructor m (ls :< a) where
    constructLayer = return ∘ wrap' ; {-# INLINE constructLayer #-}

instance Castable (Unwrapped (ls :< a)) (Unwrapped (ls' :< a')) => Castable (ls :< a) (ls' :< a') where
    cast = wrapped %~ cast ; {-# INLINE cast #-}


-- Attributes

type instance Attr a (ls :< t) = Attr a (Unwrapped (ls :< t))
instance HasAttr a (Unwrapped (ls :< t)) => HasAttr a (ls :< t) where attr = wrapped' ∘∘ attr


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

--instance ( MonadSelfBuilder s m, MonadTypeBuilder t m, Connectible t s m, MonadGraphBuilder n e m, x ~ s -- , ElemBuilder Star m x
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
        lift $ with (source ve) $ attr Succs %~ (e:)
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




