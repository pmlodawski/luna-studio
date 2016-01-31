{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Tmp where

import Prologue hiding (simple, empty, Indexable, Simple, cons, lookup, index, children, Cons, Ixed, Repr, repr, minBound, maxBound, (#), assert, Index)

import Data.Record




import Luna.Syntax.AST.Term hiding (Arrow, Node)
import qualified Luna.Syntax.AST.Term2 as T2
import Luna.Syntax.Model.Layer.Labeled


import Data.Layer.Cover
import Data.Coat
import Data.Construction

import Control.Monad.Identity
import Control.Monad.State
import Data.Container

import           Luna.Syntax.Model.Graph (Graph, GraphBuilder, MonadGraphBuilder, nodes, edges)
import qualified Luna.Syntax.Model.Graph as Graph

import Data.Construction

import Control.Monad.Reader

import qualified Luna.Syntax.Model.Builder.Type as Type
import           Luna.Syntax.Model.Builder.Type (MonadTypeBuilder, TypeBuilder, TypeBuilderT)

import Luna.Syntax.Model.Builder.Self (MonadSelfBuilder, SelfBuilderT, self, setSelf)
import qualified Luna.Syntax.Model.Builder.Self as Self

import Type.Bool



--star' :: ASTRecord '[] '[] IDT
--star' = checkedVariantCons $ Star +> 5

--star :: Lit (Labeled String (Labeled Int Cover))
--star :: Lit (Labeled String (Labeled Int) Cover)


--data IDT a = IDT a deriving (Show)





------------------------
---- === Elements === --
------------------------

----newtype Ref2 a = Ref2 a deriving (Show, Functor, Traversable, Foldable)

--data    Edge2 src tgt = Edge2 {- todo -} deriving (Show)
--data    Link t        = Link  {- todo -} deriving (Show)


--data Node2 a -- = Node2 (NetCover (TheBase a (RefCover Edge NetCover (TheBase a)))) -- deriving (Show, Functor, Traversable, Foldable)
--newtype Ref2 a = Ref2 (RefDef a)

--type family RefDef a where RefDef (Node2 a) = Netref Node (TheBase a)


----Ref2 (Node2 (Static Draft :> '[]))

--deriving instance Show (RefDef a) => Show (Ref2 a)







----type family NodeType a where NodeType (Node2 a) = a

--type family AssociatedValue t a

--type instance AssociatedValue Type a = Ref2 $ Link $ Static Draft :> '[]



--data Annotated (ts :: [*]) (a :: (* -> *) -> *) -- = Annotated (AttachmentCover ts (a (RefCover Edge NetCover a))) -- = Annotated a deriving (Show, Functor, Traversable, Foldable) 

--type family LayerTypes a where LayerTypes (Annotated ts a) = ts
--type family TheBase    a where TheBase    (Annotated ts a) = a

--type a :> lst = Annotated lst a




--type family Attachment a
--data Attached2 t a = Attached2 (Attachment (Attached2 t a)) a
--type instance Attachment (Attached2 t a) = AssociatedValue t (Uncovered a)

--type AttachmentLayer t = Layer (Attached2 t)

--type family AttachmentCover ts where
--    AttachmentCover '[]       = Cover
--    AttachmentCover (t ': ts) = AttachmentLayer t (AttachmentCover ts)





--deriving instance (Show (Attachment (Attached2 t a)), Show a) => Show (Attached2 t a)








--------------------
-- === Layers === --
--------------------

newtype Layer        l (t :: * -> *) a = Layer        (l (t a)) deriving (Show, Eq)
newtype PhantomLayer l (t :: * -> *) a = PhantomLayer l         deriving (Show, Eq, Functor, Traversable, Foldable)


-- === Instances === --

-- Wrappers

instance (Coated l, Coated t) 
                  => Coated    (Layer l t)   where coated  = wrapped  ∘ coated ∘ coated
instance Coated l => Layered   (Layer l t a) where layered = wrapped' ∘ coated ; {-# INLINE layered #-}
type instance        Unlayered (Layer l t a) = t a
instance             Rewrapped (Layer l t a) (Layer l' t' a')
instance             Wrapped   (Layer l t a) where
    type             Unwrapped (Layer l t a) = l (t a)
    _Wrapped' = iso (\(Layer a) -> a) Layer
    {-# INLINE _Wrapped' #-}

type instance Unlayered (PhantomLayer l t a) = t a
instance      Rewrapped (PhantomLayer l t a) (PhantomLayer l' t' a')
instance      Wrapped   (PhantomLayer l t a) where
    type      Unwrapped (PhantomLayer l t a) = l
    _Wrapped' = iso (\(PhantomLayer a) -> a) PhantomLayer
    {-# INLINE _Wrapped' #-}

-- Properties

type instance Attr p (Layer l t a) = Attr p (Unwrapped (Layer l t a))
instance Accessor p (Unwrapped (Layer l t a)) => Accessor p (Layer l t a) where access a = wrapped' ∘ access a

-- Refs

type instance RefOf (Layer        l t a) = RefOf (Unwrapped (Layer        l t a))
type instance RefOf (PhantomLayer l t a) = RefOf (Unwrapped (PhantomLayer l t a))

instance HasRef (Unwrapped (Layer        l t a)) => HasRef (Layer        l t a) where ref = wrapped' ∘ ref
instance HasRef (Unwrapped (PhantomLayer l t a)) => HasRef (PhantomLayer l t a) where ref = wrapped' ∘ ref





------------------------
-- === Properties === --
------------------------

data Node = Node deriving (Show)
data Edge = Edge deriving (Show)
data Type = Type deriving (Show)
data Note = Note deriving (Show)

data Successors = Successors deriving (Show)

newtype Targetting   a t = Targetting a   deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data    Attached   t d a = Attached   d a deriving (Show, Eq, Ord, Functor, Traversable, Foldable)







type family Attr a t
class Accessor a t where access :: a -> Lens' t (Attr a t)



-- === Instances === --

-- Functors

instance Bifunctor (Attached t) where bimap f g (Attached d t) = Attached (f d) (g t)

-- Wrappers

instance Rewrapped (Targetting a t) (Targetting a' t')
instance Wrapped   (Targetting a t) where
    type Unwrapped (Targetting a t) = a
    _Wrapped' = iso (\(Targetting a) -> a) Targetting ; {-# INLINE _Wrapped' #-}

instance Coated (Attached t d) where coated = lens (\(Attached _ t) -> t) (\(Attached d _) t -> Attached d t)

-- Targets

type instance Target (Targetting a t) = t

-- Conversions

instance (Castable d d', Castable a a') => Castable (Attached t d a) (Attached t' d' a') where cast (Attached d a) = Attached (cast d) (cast a)

-- Defaults

instance {-# OVERLAPPABLE #-} (Default d, Monad m) => CoatConstructor a m (Attached Note d) where constructCoat = return ∘ Attached def

instance {-# OVERLAPPABLE #-} 
         ( 
           MonadFix m
         , MonadTypeBuilder t m
         , EdgeBuilder t t m
         , Builder Edge conn m
         , EdgeBetween t t ~ conn
         , MonadSelfBuilder t m
         , (Castable (Layer (Attached Successors [Netref Edge (Static Draft)]) (Attached' Type (Netref Edge (Static Draft)) Cover) (Static Draft (RefCover Edge (Attached' Successors [Netref Edge (Static Draft)] (Attached' Type (Netref Edge (Static Draft)) Cover)) (Static Draft)))) n)
         , (Builder Node (Netref Node (Static Draft)) m)
         , (OutputRegister (Netref Node (Static Draft)) (Netref Edge (Static Draft)) m)
         , t    ~ Netref Node (Static Draft)
         , conn ~ Netref Edge (Static Draft)
         , MonadGraphBuilder n e m
         , (Castable n (Layer (Attached Successors [Netref Edge (Static Draft)]) (Attached' Type (Netref Edge (Static Draft)) Cover) (Static Draft (RefCover Edge (Attached' Successors [Netref Edge (Static Draft)] (Attached' Type (Netref Edge (Static Draft)) Cover)) (Static Draft)))))
         ) => CoatConstructor a m (Attached Type conn) where 
    constructCoat a = flip Attached a <$> do
        s <- self
        Type.ask >>= \case
            Just (t :: t)  -> do
                c <- connection2 t s
                return c
            Nothing -> mdo
                Type.set t
                t <- star
                c <- connection t s

                tv <- readRef t

                registerOutput t (tv ^. access Type)

                return c



instance Monad m => CoatConstructor a m (Attached Successors [conn]) where constructCoat = return ∘ Attached []

-- Attributes

type instance Attr p (Attached t d a) = If (p == t) d (Attr p a)
instance {-# OVERLAPPABLE #-}                                                       Accessor t (Attached t d a) where access _ = lens (\(Attached d _) -> d) (\(Attached _ a) d -> Attached d a)
instance {-# OVERLAPPABLE #-} (Accessor p a, Attr p (Attached t d a) ~ Attr p a) => Accessor p (Attached t d a) where access a = coated ∘ access a


--star :: (MonadFix m, MonadSelfBuilder a m, CoverConstructor m a, Builder Node a m, Uncovered a ~ Static Draft t) => m a


--instance {-# OVERLAPPABLE #-} 
--         ( Monad m
--         ) => CoatConstructor a m (Attached Type String) where 
--    constructCoat a = flip Attached a <$> do
--        return ""


------------------
-- === Refs === --
------------------

data Ref r a = Ref a deriving (Show, Eq, Functor, Traversable, Foldable)

-- FIXME[WD]: refactor `Target` from Term.hs:
--type family Target a -- using the one defined in Term.hs
type family RefOf  a
class HasRef a where ref :: Lens' a (RefOf a)
 
class RefGetter ref m where getRef :: ref -> m (Target ref)
class RefPutter ref m where putRef :: ref -> Target ref -> m ()

readRef :: (HasRef r, RefGetter ref m, ref ~ RefOf r) => r -> m (Target ref)
readRef  = getRef ∘ view ref

writeRef :: (HasRef r, RefPutter ref m, ref ~ RefOf r) => r -> Target ref -> m ()
writeRef r = putRef (r ^. ref) 

follow (Arc src tgt) = readRef tgt


-- === Instances === --

type instance Target     (Ref r a)                = Target r
type instance Destructed (Ref (Targetting r t) a) = t

type instance RefOf  (Ref t a) = Ref t a
instance      HasRef (Ref t a) where ref = id

-- Wrappers

instance Rewrapped (Ref r a) (Ref r' a')
instance Wrapped   (Ref r a) where
    type Unwrapped (Ref r a) = a
    _Wrapped' = iso (\(Ref a) -> a) Ref


instance (a ~ Int, MonadGraphBuilder n e m, Castable n t)
      => RefGetter (Ref (Targetting Node t) a) m where
    getRef ref = do
        g <- Graph.get
        let d   = index_ (unwrap' ref) $ g ^. nodes
            out = cast d
        return out

instance (a ~ Int, MonadGraphBuilder n e m, Castable e t)
      => RefGetter (Ref (Targetting Edge t) a) m where
    getRef ref = do
        g <- Graph.get
        let d   = index_ (unwrap' ref) $ g ^. edges
            out = cast d
        return out

instance (a ~ Int, MonadGraphBuilder n e m, Castable t n)
      => RefPutter (Ref (Targetting Node t) a) m where
    putRef ref val = do
        (g :: Graph n e) <- Graph.get
        let g' = g & nodes %~ unchecked inplace insert_ (unwrap' ref :: Int) (cast val) :: Graph n e
        Graph.put g'
        return ()
        --Builder.modify_ $ nodes %~ unchecked inplace insert_ (deref ptr) a


instance (a ~ Int, MonadGraphBuilder n e m, Castable t n)
      => Constructor m (Ref (Targetting Node t) a) where
    construct ast = Ref <$> Graph.modify (nodes $ swap ∘ ixed add (cast ast))

-- Conversions

instance Castable    (Unwrapped (Layer l t a)) (Unwrapped (Layer l' t' a')) => Castable    (Layer l t a) (Layer l' t' a') where cast    = wrapped %~ cast
instance Convertible (Unwrapped (Layer l t a)) (Unwrapped (Layer l' t' a')) => Convertible (Layer l t a) (Layer l' t' a') where convert = wrapped %~ convert



-------------------
-- === Edges === --
-------------------

data Arrow       tgt = Arrow   tgt deriving (Show, Functor, Traversable, Foldable)
data Arc     src tgt = Arc src tgt deriving (Show, Functor, Traversable, Foldable)
type HomoArc t       = Arc t   t

class EdgeCons src tgt edge where consEdge :: src -> tgt -> edge

-- === Construction === --

arc     :: HaveRef '[src,tgt] => src -> tgt -> Arc (RefOf src) (RefOf tgt)
homoArc :: HasRef t           => t   -> t   -> HomoArc (RefOf t)
arc src tgt = Arc (src ^. ref) (tgt ^. ref) ; {-# INLINE arc     #-}
homoArc     = arc                           ; {-# INLINE homoArc #-}

arrow :: HasRef tgt => tgt -> Arrow (RefOf tgt)
arrow tgt = Arrow (tgt ^. ref) ; {-# INLINE arrow #-}

-- === Instances === --

-- Functors

instance Bifunctor Arc where bimap f g (Arc a b) = Arc (f a) (g b) ; {-# INLINE bimap #-}

-- EdgeCons

instance (HaveRef '[src,tgt], srcRef ~ RefOf src, tgtRef ~ RefOf tgt) => EdgeCons src tgt (Arc   srcRef tgtRef) where consEdge   = arc   ; {-# INLINE consEdge #-}
instance (HasRef tgt, tgtRef ~ RefOf tgt)                             => EdgeCons src tgt (Arrow        tgtRef) where consEdge _ = arrow ; {-# INLINE consEdge #-}








instance Coated Cover          where coated = lens (\(Cover a) -> a) (const Cover)





--instance (CoatConstructor m l, Functor m) => LayerConstructor m (Layer l t a) where constructLayer = Layer <∘> constructCoat
instance (Functor m, CoatConstructor (t a) m l)           => LayerConstructor m (Layer        l t a) where constructLayer = Layer        <∘> constructCoat
instance (Functor m, Destructed l ~ t a, Constructor m l) => LayerConstructor m (PhantomLayer l t a) where constructLayer = PhantomLayer <∘> construct






infixl 9 :<
type l :< t = Layer l t



--cons' :: SmartCons (Cons n t) b => n -> [t] -> b
--cons' = cons ∘∘ Cons


data Test a b = Test !a !b  deriving (Show)

type family HaveRef lst :: Constraint where 
    HaveRef '[]       = ()
    HaveRef (a ': as) = (HasRef a, HaveRef as)



type instance RefOf (Targetting a t)   = RefOf a

instance HasRef (Unwrapped (Targetting a t))   => HasRef (Targetting a t)   where ref = wrapped' ∘ ref





newtype TypeConstraint  t (tp :: *)      m a = TypeConstraint  (m a) 

constrainType :: t -> Proxy tp -> TypeConstraint t tp m a -> m a
constrainType _ _ (TypeConstraint ma) = ma


constrainType' (TypeConstraint ma) = ma

instance Functor m => Functor (TypeConstraint t tp m) where
    fmap f (TypeConstraint ma) = TypeConstraint $ fmap f ma

instance Applicative m => Applicative (TypeConstraint t tp m) where
    pure = TypeConstraint ∘ pure
    TypeConstraint f <*> TypeConstraint ma = TypeConstraint $ f <*> ma

instance Monad m => Monad (TypeConstraint t tp m) where
    return = TypeConstraint ∘ return
    (TypeConstraint a) >>= f = TypeConstraint $ a >>= (fmap constrainType' f)

instance MonadFix m => MonadFix (TypeConstraint t tp m) where
    mfix f = TypeConstraint $ mfix (fmap constrainType' f)

instance MonadTrans (TypeConstraint t tp) where
    lift = TypeConstraint

instance MonadIO m => MonadIO (TypeConstraint t tp m) where
    liftIO = TypeConstraint ∘ liftIO


--instance {-# OVERLAPPABLE #-} (Monad m, Builder t (a x) m, (tp) ~ (a)) => Builder t (a x) (TypeConstraint t (tp y) m) where register = lift ∘∘ register
instance {-# OVERLAPPABLE #-} (Monad m, Builder t (a x) m, b ~ a x, tp ~ tpa (tpx :: (* -> *) -> *), tpa ~ a) => Builder t b (TypeConstraint t tp m) where register = lift ∘∘ register
instance {-# OVERLAPPABLE #-} (Monad m, Builder t a m)         => Builder t a (TypeConstraint t' tp m) where register = lift ∘∘ register






type family Proxified p where
    Proxified (Proxy p) = Proxy p
    Proxified a         = Proxy a


class Proxify p where proxify :: p -> Proxified p

instance {-# OVERLAPABLE #-}                          Proxify (Proxy p) where proxify = id
instance {-# OVERLAPABLE #-} Proxified a ~ Proxy a => Proxify a         where proxify = valProxy

valProxy :: a -> Proxy a
valProxy _ = Proxy

------------------------------------------------------------------------------------------------------------------------------------


------------------------------
-- === Graph references === --
------------------------------

-- === Structures === --

data family Netref   (t :: *)                   (base :: (* -> *) -> *)
data family RefCover (t :: *) (cover :: * -> *) (base :: (* -> *) -> *) (a :: *)

newtype instance Netref Node n = Netref_Node (Unlayered (Netref Node n)) deriving (Show, Eq)
newtype instance Netref Edge n = Netref_Edge (Unlayered (Netref Edge n)) deriving (Show, Eq)
type    instance Unlayered (Netref Node n) = (RefCover Node NetCover n) (n (RefCover Edge NetCover n))
type    instance Unlayered (Netref Edge n) = (RefCover Edge NetCover n) (n (RefCover Edge NetCover n))

newtype instance RefCover Node cover base a = RefCover_Node (Unlayered (RefCover Node cover base a)) deriving (Show, Eq)
newtype instance RefCover Edge cover base a = RefCover_Edge (Unlayered (RefCover Edge cover base a)) deriving (Show, Eq)
type instance    Unlayered (RefCover Node cover n a) = TargetGenRef Node (cover   a) cover a
type instance    Unlayered (RefCover Edge cover n a) = TargetGenRef Edge (NetArcT n) cover a

type NetArcT      n       = Arc (Netref Node n) (Netref Node n)
type TargetGenRef r x t a = PhantomLayer (Ref (Targetting r x) Int) t a


-- === Instances === --

-- Wrappers

instance Rewrapped (Netref Node n) (Netref Node n')
instance Wrapped   (Netref Node n) where
    type Unwrapped (Netref Node n) = Unlayered (Netref Node n)
    _Wrapped' = iso (\(Netref_Node a) -> a) Netref_Node

instance Rewrapped (Netref Edge n) (Netref Edge n')
instance Wrapped   (Netref Edge n) where
    type Unwrapped (Netref Edge n) = Unlayered (Netref Edge n)
    _Wrapped' = iso (\(Netref_Edge a) -> a) Netref_Edge

instance Rewrapped (RefCover Node cover n a) (RefCover Node cover' n' a')
instance Wrapped   (RefCover Node cover n a) where
    type Unwrapped (RefCover Node cover n a) = Unlayered (RefCover Node cover n a)
    _Wrapped' = iso (\(RefCover_Node a) -> a) RefCover_Node

instance Rewrapped (RefCover Edge cover n a) (RefCover Edge cover' n' a')
instance Wrapped   (RefCover Edge cover n a) where
    type Unwrapped (RefCover Edge cover n a) = Unlayered (RefCover Edge cover n a)
    _Wrapped' = iso (\(RefCover_Edge a) -> a) RefCover_Edge

-- References

type instance RefOf  (Netref   Node       n)   = RefOf (Unwrapped (Netref Node n))
type instance RefOf  (Netref   Edge       n)   = RefOf (Unwrapped (Netref Edge n))
type instance RefOf  (RefCover Node cover n a) = RefOf (Unwrapped (RefCover Node cover n a))
type instance RefOf  (RefCover Edge cover n a) = RefOf (Unwrapped (RefCover Edge cover n a))
instance      HasRef (Netref   Node       n)   where ref = wrapped' ∘ ref
instance      HasRef (Netref   Edge       n)   where ref = wrapped' ∘ ref
instance      HasRef (RefCover Node cover n a) where ref = wrapped' ∘ ref
instance      HasRef (RefCover Edge cover n a) where ref = wrapped' ∘ ref

-- Construction

instance Monad m => LayerConstructor m (Netref   Node       n)   where constructLayer = return ∘ Netref_Node
instance Monad m => LayerConstructor m (Netref   Edge       n)   where constructLayer = return ∘ Netref_Edge
instance Monad m => LayerConstructor m (RefCover Node cover n a) where constructLayer = return ∘ RefCover_Node
instance Monad m => LayerConstructor m (RefCover Edge cover n a) where constructLayer = return ∘ RefCover_Edge




------------------------------------------------------------------------------------------------------------------------------------















--newtype TargetGenRef r x t a = TargetGenRef (PhantomLayer (Ref (Targetting r (x a)) Int) t a) deriving (Show)



-----------------------
-- === TargetRef === --
-----------------------

newtype TargetRef r t a = TargetRef (PhantomLayer (Ref (Targetting r (t a)) Int) t a) deriving (Show)

type instance RefOf  (TargetRef r t a) = RefOf (Unwrapped (TargetRef r t a))
instance      HasRef (TargetRef r t a) where ref = wrapped' ∘ ref



-- === Instances === --


-- Wrappers
type instance Unlayered (TargetRef r t a) = Unwrapped (TargetRef r t a)
instance      Rewrapped (TargetRef r t a) (TargetRef r' t' a')
instance      Wrapped   (TargetRef r t a) where
    type      Unwrapped (TargetRef r t a) = PhantomLayer (Ref (Targetting r (t a)) Int) t a
    _Wrapped' = iso (\(TargetRef a) -> a) TargetRef

-- Layers
instance Monad m => LayerConstructor m (TargetRef r t a) where constructLayer = return ∘ TargetRef



------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------
-- ========================== --
-- === Elementary Builder === --
-- ========================== --


class Monad m => Builder t a m where 
    -- | The `register` function can be used to indicate that a particular element is "done".
    --   It does not provide any general special meaning. In general, this information can be lost when not used explicitly.
    --   For a specific usage look at the `Network` builder, where `register` is used to add type constrains on graph nodes and edges.
    --   The `t` parameter is the type of registration, like `Node` or `Edge`. Please keep in mind, that `Node` indicates a "kind" of a structure.
    --   It does not equals a graph-like node - it can be a "node" in flat AST representation, like just an ordinary term.
    register :: Proxy t -> a -> m ()

instance Builder t a m => Builder t a (Graph.GraphBuilderT n e m) where register = lift ∘∘ register
instance Builder t a m => Builder t a (StateT                s m) where register = lift ∘∘ register
instance Builder t a m => Builder t a (TypeBuilderT          s m) where register = lift ∘∘ register
instance Builder t a m => Builder t a (SelfBuilderT          s m) where register = lift ∘∘ register
instance                  Builder t a IO                          where register _ _ = return ()
instance                  Builder t a Identity                    where register _ _ = return ()

registerOverM :: Builder t a m => Proxy t -> m a -> m a
registerOverM p ma = do
    a <- ma
    register p a
    return a

registerOver :: Builder t a m => Proxy t -> a -> m a
registerOver p = registerOverM p ∘ return

registerNode :: Builder Node a m => a -> m a
registerEdge :: Builder Edge a m => a -> m a
registerNode = registerOver (Proxy :: Proxy Node)
registerEdge = registerOver (Proxy :: Proxy Edge)



------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------






type family EdgeBetween src tgt
class EdgeBuilder src tgt m where edge :: src -> tgt -> m (EdgeBetween src tgt)

type instance EdgeBetween (Netref Node n) (Netref Node n) = Netref Edge n
instance (EdgeBuilder' (Netref Node n) (Netref Node n) m (Unwrapped (Netref Edge n)), Functor m) => EdgeBuilder (Netref Node n) (Netref Node n) m where edge = Netref_Edge <∘∘> edge'


-- Helpers
-- TODO[WD]: Refactor the `EdgeBuilder'` class. It is just a helper one and should be removed or cleaned.
--           It does not preserve the type dependenices set by `EdgeBuilder`

class EdgeBuilder' src tgt m e  where edge' :: src -> tgt -> m e

instance (MonadGraphBuilder n e m, Convertible (Arc (RefOf src) (RefOf tgt)) e, HaveRef '[src,tgt], a ~ Int) => EdgeBuilder' src tgt m (Ref r a) where
    edge' src tgt = Ref <$> Graph.modify (edges $ swap ∘ ixed add (convert $ arc src tgt))

instance (Functor m, EdgeBuilder' src tgt m (Ref (Targetting r (t a)) Int))           => EdgeBuilder' src tgt m (TargetRef r t a) where edge' = (TargetRef  ∘ PhantomLayer) <∘∘> edge'
instance (Functor m, EdgeBuilder' src tgt m (Unwrapped (Unwrapped (RefCover Edge cover n a)))) => EdgeBuilder' src tgt m (RefCover Edge cover n a)  where edge' = (RefCover_Edge ∘ PhantomLayer) <∘∘> edge'







instance (Convertible src src', Convertible tgt tgt') => Convertible (Arc src tgt) (Arc src' tgt') where convert = bimap convert convert


instance Convertible a a' => Convertible (Ref r a) (Ref r' a') where convert (Ref a) = Ref $ convert a
--instance 


connection :: (EdgeBuilder src tgt m, Builder Edge e m, e ~ EdgeBetween src tgt, OutputRegister src e m) => src -> tgt -> m e
connection src tgt = do
    conn <- registerEdge =<< edge src tgt
    registerOutput src conn
    return conn

connection2 :: (EdgeBuilder src tgt m, Builder Edge e m, e ~ EdgeBetween src tgt) => src -> tgt -> m e
connection2 src tgt = do
    conn <- registerEdge =<< edge src tgt
    --registerOutput src conn
    return conn

class OutputRegister n conn m where registerOutput :: n -> conn -> m ()

instance ( rn   ~ Netref Node a
         , rc   ~ Netref Edge a
         , refn ~ RefOf rn
         , Monad m
         , HasRef rn
         , RefGetter refn m
         , RefPutter refn m
         , Accessor Successors (Target refn)
         , Attr Successors (Target refn) ~ [rc]
         ) => OutputRegister (Netref Node a) (Netref Edge a) m where
    registerOutput rn rc = do
        n <- readRef rn
        let x = n & (access Successors) %~ (rc:)
        writeRef rn x




instance (Castable src src', Castable tgt tgt') => Castable (Arc src tgt) (Arc src' tgt') where cast = bimap cast cast

instance Castable (Ref r a) (Unwrapped (Unwrapped (Unwrapped (Netref Edge n)))) => Castable (Ref r a) (Netref Edge n) where cast = wrap' ∘ wrap' ∘ wrap' ∘ cast
instance Castable (Ref r a) (Unwrapped (Unwrapped (Unwrapped (Netref Node n)))) => Castable (Ref r a) (Netref Node n) where cast = wrap' ∘ wrap' ∘ wrap' ∘ cast

instance Castable (Ref r a) (Ref r' a) where cast = rewrap

--type family Netref t (n :: (* -> *) -> *) :: *

--type instance Netref Node n = Netref Node n
--type instance Netref Edge n = Netref Edge n


-- FIXME[WD]: remove the hardcoded `NetCover`
instance (a' ~ n (RefCover Edge cover n), n ~ n', cover ~ NetCover) => Convertible (Netref Edge n) (RefCover Edge cover n' a') where convert = unwrap'




--data family Foo a 

--data instance Foo Int = FooInt Int deriving (Show)


-- ========================== --
-- === AST building utils === --
-- ========================== --

buildMe f = mdo
    setSelf me
    me <- f
    return me

starLit :: Lit t
starLit = cons Star

--star' :: (CoverConstructor m a, Builder Node a m, Uncovered a ~ Lit t) => m a
--star' = registerNode =<< constructCover star


starDraft :: Static Draft t
starDraft = cons starLit

--star :: (MonadFix m, MonadSelfBuilder a m, CoverConstructor m a, Builder Node a m, Uncovered a ~ Static Draft t) => m a
star = registerNode =<< buildMe (constructCover starDraft)
--star = registerNode =<< constructCover starx




unifyx :: t (Static Draft t) -> t (Static Draft t) -> Static Draft t
unifyx a b = cons $ Unify a b

--unifyx2 :: (CoverConstructor m a, Builder Node a m, Uncovered a ~ Static Draft t) => t (Static Draft t) -> t (Static Draft t) -> m a
--unifyx2 a b = registerNode =<< constructCover (unifyx a b)
 
unify a b = mdo
    ca  <- connection a out
    cb  <- connection b out
    out <- registerNode =<< buildMe (constructCover (unifyx (convert ca) (convert cb)))
    return out



-- =============== --
-- === Network === --
-- =============== --


type Network  = Graph (NetCover Data) NetArc
type NetArc   = Arc (Ref Node Int) (Ref Node Int)
type NetCover = Attached' Successors [Netref Edge (Static Draft)] (Attached' Type (Netref Edge (Static Draft)) Cover)

type Attached' t d = Layer (Attached t d)






--type NetCover = Attached' Type (Netref Edge (Static Draft)) Cover

-- === Construction === ---

--buildNetwork :: _ => _
buildNetwork  = runIdentity ∘ buildNetworkM
buildNetworkM = rebuildNetworkM def
rebuildNetworkM (net :: Network) = flip Self.evalT (undefined :: Netref Node (Static Draft))
                                 ∘ flip Type.evalT (Nothing :: Maybe (Netref Node (Static Draft)))
                                 ∘ constrainType Node (Proxy :: Proxy (Netref Node n))
                                 ∘ constrainType Edge (Proxy :: Proxy (Netref Edge e))
                                 ∘ flip Graph.execT net
{-# INLINE   buildNetworkM #-}
{-# INLINE rebuildNetworkM #-}




-------------------------------------------------------------------------------------------------------------------
-- TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST --
-------------------------------------------------------------------------------------------------------------------

ucase = caseTest ∘ uncover

class MyShow a b where myShow :: a -> b

instance (Show a, b ~ String) => MyShow a b where myShow = show




--test :: forall x. x -> String
--test _ = "ala" where
--    a   = cons $ Var (Str "a") :: Static Draft IDT
--    u   = cons $ Unify (IDT a) (IDT a) :: Static Draft IDT
--    foo = withElement_ (p :: P (TFoldable x)) (foldrT ((:) :: x -> [x] -> [x]) []) u


--class WithElement_ ctx rec where withElement_ :: Proxy ctx -> (forall v. ctx v => v -> a) -> rec -> a

--main :: IO ()
--main = do
--    let a = cons $ Var (Str "a") :: Static Draft IDT
--        u = cons $ Unify (IDT a) (IDT a) :: Static Draft IDT
--        --v = (1 :: Int) :: x
--        --out = withElement_ (p :: P (TFoldable x)) (foldrT ((:) :: x' -> [x'] -> [x']) []) u
--    print a

--    print $ test u

--    return ()
