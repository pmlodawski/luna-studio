{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}

{-# LANGUAGE PartialTypeSignatures     #-}

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prologue hiding (simple, empty, Indexable, Simple, cons, lookup, index, children, Cons, Ixed, Repr, repr, minBound, maxBound, (#), assert, Index)

import Data.Record




import Luna.Syntax.AST.Term hiding (Arrow, Node)
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




--------------------
-- === Layers === --
--------------------

newtype Layer        l (t :: * -> *) a = Layer        (l (t a)) deriving (Show)
newtype PhantomLayer l (t :: * -> *) a = PhantomLayer l         deriving (Show, Functor, Traversable, Foldable)

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

newtype Targetting   a t = Targetting a   deriving (Show, Functor, Traversable, Foldable)
data    Attached   t d a = Attached   d a deriving (Show, Functor, Traversable, Foldable)


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
         , Builder Node t m
         , Uncovered t ~ Static Draft x
         , MonadTypeBuilder t m
         , EdgeBuilder t t m
         , Builder Edge conn m
         , EdgeBetween t t ~ conn
         , (Uncovered (Unlayered t) ~ Static Draft x)
         , t ~ Netref Node (Static Draft)
         , MonadGraphBuilder n e m

         , (Castable (Layer (Attached Type (Netref Edge (Static Draft))) Cover (Static Draft (RefCover Edge (Attached' Type (Netref Edge (Static Draft)) Cover) (Static Draft)))) n)
         , conn ~ Netref Edge (Static Draft)
         , MonadSelfBuilder t m
         ) => CoatConstructor a m (Attached Type conn) where 
    constructCoat a = flip Attached a <$> do
        s <- self
        Type.ask >>= \case
            Just t  -> do
                c <- connection s t
                return c
            Nothing -> mdo
                Type.set t
                t <- starx'
                c <- connection s t
                return c

-- Attributes

type instance Attr p (Attached t d a) = If (p == t) d (Attr p a)
instance {-# OVERLAPPABLE #-}                                                       Accessor t (Attached t d a) where access _ = lens (\(Attached d _) -> d) (\(Attached _ a) d -> Attached d a)
instance {-# OVERLAPPABLE #-} (Accessor p a, Attr p (Attached t d a) ~ Attr p a) => Accessor p (Attached t d a) where access a = coated ∘ access a

--starx' :: (CoverConstructorFix m a, Builder Node a m, Uncovered a ~ Static Draft t) => m a


--instance {-# OVERLAPPABLE #-} 
--         ( Monad m
--         ) => CoatConstructor a m (Attached Type String) where 
--    constructCoat a = flip Attached a <$> do
--        return ""


------------------
-- === Refs === --
------------------

data Ref r a = Ref a deriving (Show, Functor, Traversable, Foldable)

-- FIXME[WD]: refactor `Target` from Term.hs:
--type family Target a -- using the one defined in Term.hs
type family RefOf  a
class HasRef a where ref :: Lens' a (RefOf a)
 
class RefGetter ref m where getRef :: ref -> m (Target ref)

readRef = getRef ∘ view ref


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

newtype instance Netref Node n = Netref_Node (Unlayered (Netref Node n)) deriving (Show)
newtype instance Netref Edge n = Netref_Edge (Unlayered (Netref Edge n)) deriving (Show)
type    instance Unlayered (Netref Node n) = (RefCover Node NetCover n) (n (RefCover Edge NetCover n))
type    instance Unlayered (Netref Edge n) = (RefCover Edge NetCover n) (n (RefCover Edge NetCover n))

newtype instance RefCover Node cover base a = RefCover_Node (Unlayered (RefCover Node cover base a)) deriving (Show)
newtype instance RefCover Edge cover base a = RefCover_Edge (Unlayered (RefCover Edge cover base a)) deriving (Show)
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









type Attached' t d = Layer (Attached t d)






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


connection :: (EdgeBuilder src tgt m, Builder Edge e m, e ~ EdgeBetween src tgt) => src -> tgt -> m e
connection src tgt = registerEdge =<< edge src tgt




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

star :: Lit t
star = cons Star

star' :: (CoverConstructorFix m a, Builder Node a m, Uncovered a ~ Lit t) => m a
star' = registerNode =<< constructCoverFix star


starx :: Static Draft t
starx = cons star

starx' :: (MonadFix m, MonadSelfBuilder a m, CoverConstructorFix m a, Builder Node a m, Uncovered a ~ Static Draft t) => m a
starx' = registerNode =<< buildMe (constructCoverFix starx)




unifyx :: t (Static Draft t) -> t (Static Draft t) -> Static Draft t
unifyx a b = cons $ Unify a b

--unifyx2 :: (CoverConstructorFix m a, Builder Node a m, Uncovered a ~ Static Draft t) => t (Static Draft t) -> t (Static Draft t) -> m a
--unifyx2 a b = registerNode =<< constructCoverFix (unifyx a b)
 
unifyx' a b = mdo
    ca  <- connection a out
    cb  <- connection b out
    out <- registerNode =<< buildMe (constructCoverFix (unifyx (convert ca) (convert cb)))
    return out



-- =============== --
-- === Network === --
-- =============== --


type Network  = Graph (NetCover Data) NetArc
type NetArc   = Arc (Ref Node Int) (Ref Node Int)
type NetCover = Attached' Type (Netref Edge (Static Draft)) Cover

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


data IDT a = IDT a deriving (Show)


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


main :: IO ()
main = do

        --s = star
    --g <- flip evalStateT (0 :: Int) $ flip Graph.execT (def :: Network) $ do
       -- $ constrainCoverType (Proxy :: Proxy (Ref' Node (Attached' String Cover)))

    g <- buildNetworkM $ do
        --x <- constructCoverFix star :: _ (TargetRef Node (Attached' String Cover) (Lit (TargetRef Edge  (Attached' String Cover))))


        s1 <- starx'
        s2 <- starx'
        print s1


        c <- connection s1 s2
        print c


        s1_v <- readRef s1
        c_v <- readRef c

        print s1_v
        print c_v

        uu <- unifyx' s1 s2

--        --print (withElement' (p :: P MyShow) myShow $ uncover s1_v)
        uu_v <- readRef uu


        print $ view (access Type) uu_v
--        --print (uncover uu_v :: Static Draft (RefCover Edge (Attached' Type (Netref Node (Static Draft)) Cover) (Static Draft)))
--        --print $ withElement_ (p :: P (TFoldable (Static Draft (RefCover Edge (Attached' Type (Netref Node (Static Draft)) Cover) (Static Draft))))) (foldrT (:) []) uu_v
--        --u <- unifyx' s1 s2
----class WithElement_ ctx rec where withElement_ :: Proxy ctx -> (forall v. ctx v => v -> a) -> rec -> a

        print $ (inputs $ uncover uu_v)

--        --print $ ucase s1_v $ do
--        --    match $ \(Lit _) -> "Its a Lit!"
--        --    match $ \ANY     -> "Something else"


        return ()

        --s' = s & coated %~ unwrap' ∘ unwrap'

    print $ (g :: Network)
    return ()

    --print $ caseTest t1 $ do
    --    --match $ \Star    -> "star!"
    --    --dynamic $ \s -> "its dynamic! :O"
    --    static $ \s -> "it is static!  :O"
    --    match  $ \(Cons _ _) -> "its cons ..."
    --    --match  $ \(Lit l) -> caseTest l $ do
    --    --    match $ \Star -> "its star!"
    --    match $ \ANY     -> "something else"
    



--    return ()


-- time  take  -  description                              FIXME
----------------------------------------------------------------
--           [+] readRef dla Edge               
-- 0:35  30  [+] automatic constructed connection type
-- 2:18  30  [?] types
-- 4:11  30  [+] predecessors
--       30  [+] attach accessors
--       30  [ ] successors
--       30  [ ] destructors
--       30  [ ] term construction methods
--       30  [ ] nice connect / reconnect
--
-- [ ] magic monad builder
-- [ ] pretty TH case
-- [ ] 



-------------------------
-- === Benchmarks === ---
-------------------------


--data Bench a = Bench1 a
--             | Bench2
--             deriving (Show)

--main = do


--    args <- getArgs
--    let mode   = read (args !! 0) :: Int
--        argnum = read (args !! 1) :: Int
--        nums = [0..argnum]


--    case mode of
--        0 -> do
--            let ls = const star . show <$> nums
--                pattest l = caseTest l $ do
--                    variantMatch (\Star -> (1 :: Int))
--                getnum _ = 0
--            print $ sum $ pattest <$> ls
--            --print $ sum $ getnum <$> ls
--        1 -> do
--            let ls = const Bench2 . show <$> nums
--                pattest l = case l of
--                    Bench2 -> (1 :: Int)
--                getnum _ = 0
--            print $ sum $ pattest <$> ls
--            --print $ sum $ getnum <$> ls


-- === Performance notes === ---
-- Performance drops observed:
--     - using custom State class and a wrapper for pattern-matches causes drop
--       probably because automatically derived methods in the State wrapper are not inlined (TBI).
--     - using the `reverse` function in pattern match causes a drop, but it should be computed always during the compile time.