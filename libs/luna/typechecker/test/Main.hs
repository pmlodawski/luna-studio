{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RecursiveDo               #-}

module Main where

import Prologue hiding (simple, empty, Indexable, Simple, cons, lookup, index, children, Cons, Ixed, Repr, repr, minBound, maxBound, (#), assert, Index)

import Data.Record




import Luna.Syntax.AST.Term hiding (Arrow)
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
         ( CoverConstructorFix m t, Builder Node t m, Uncovered t ~ Static Draft x
         , MonadTypeBuilder t m, MonadFix m
         , Typeable d --x
         ) => CoatConstructor a m (Attached Type d) where 
    constructCoat a = flip Attached a <$> do
        Type.ask >>= \case
            Just t  -> return t
            Nothing -> mdo
                Type.set t
                t <- starx' :: m t
                return t

        error (show $ typeOf (Proxy :: Proxy d))
        ----return ∘ Attached def
        --undefined


------------------
-- === Refs === --
------------------

data Ref r a = Ref a deriving (Show, Functor, Traversable, Foldable)

type family Target a
type family RefOf  a
class HasRef a where ref :: Lens' a (RefOf a)
 
class RefGetter ref m where getRef :: ref -> m (Target ref)

readRef = getRef ∘ view ref


-- === Instances === --

type instance Target     (Ref r a)           = Target r
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



cons' :: SmartCons (Cons n t) b => n -> [t] -> b
cons' = cons ∘∘ Cons

caseTest = __case__ "tc-test" "test/Main.hs" 0
{-# INLINE caseTest #-}

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
------------------------------------------------------------------------------------------------------------------------------------
-- =============== --
-- === Network === --
-- =============== --

--type NetRef     tp tgt = Ref tp tgt Int
--type NetFreeRef tp     = NetRef tp 'Nothing

type NetNodeRef n = TargetRef Node (Attached' Type Char Cover) (n (TargetRef Edge (Attached' Type Char Cover)))
type NetEdgeRef n = TargetRef Edge (Attached' Type Char Cover) (n (TargetRef Edge (Attached' Type Char Cover)))

type NetNodeRefTst n = TargetRef Node Cover (n (TargetRef Edge Cover))


--newtype XNetNodeRef (n :: (* -> *) -> *) = XNetNodeRef ( (NodeRefPfx n) (n (NodeRefPfx n)) )
--newtype XNetEdgeRef (n :: (* -> *) -> *) = XNetEdgeRef ( (EdgeRefPfx n) (n (EdgeRefPfx n)) )

--type    NodeRefPfx  (n :: (* -> *) -> *) = TargetRef Node (NetCover n)
--type    EdgeRefPfx  (n :: (* -> *) -> *) = TargetGenRef (NetArcT n) Edge (NetCover n)

--type    NetCover    (n :: (* -> *) -> *) = Attached' Type (XNetNodeRef n) Cover

--type NetArcT  (n :: (* -> *) -> *) = Arc (XNetNodeRef n) (XNetNodeRef n)



newtype XNetNodeRef (n :: (* -> *) -> *) = XNetNodeRef ( (NodeRefPfx n) (n (EdgeRefPfx n)) ) deriving (Show)
newtype XNetEdgeRef (n :: (* -> *) -> *) = XNetEdgeRef ( (EdgeRefPfx n) (n (EdgeRefPfx n)) ) deriving (Show)

newtype NodeRefPfx  (n :: (* -> *) -> *) a = NodeRefPfx ( TargetGenRef2 Node (NetCover n a) (NetCover n) a ) deriving (Show)
newtype EdgeRefPfx  (n :: (* -> *) -> *) a = EdgeRefPfx ( TargetGenRef2 Edge (NetArcT  n  ) (NetCover n) a ) deriving (Show)

type NodeRefPfx' a =  TargetGenRef2 Node (Cover a) Cover a

--type    NetCover    (n :: (* -> *) -> *) = Attached' Type (XNetNodeRef n) Cover
type    NetCover    (n :: (* -> *) -> *) = Cover

type NetArcT  (n :: (* -> *) -> *) = Arc (XNetNodeRef n) (XNetNodeRef n)

type TargetGenRef2 r x t a = PhantomLayer (Ref (Targetting r x) Int) t a





type Network    = Graph (NetWrapper Data) NetArc
type NetWrapper = Cover
type NetArc     = Arc (Ref Node Int) (Ref Node Int)
type NetRef r   = Ref r Int

type Attached' t d = Layer (Attached t d)


type NodeRef w n = TargetRef Node w (n (TargetRef Edge w))
type EdgeRef w e = TargetRef Edge w (e (TargetRef Edge w))

--type NetNodeRef n = NodeRef NetWrapper n
--type NetEdgeRef e = EdgeRef NetWrapper e

-- === Construction === ---
buildNetwork :: _ => _
buildNetwork  = runIdentity ∘ buildNetworkM
buildNetworkM = rebuildNetworkM def
rebuildNetworkM (net :: Network) = flip Type.evalT Nothing
                                 ∘ constrainType Node (Proxy :: Proxy (XNetNodeRef n))
                                 --constrainType Node (Proxy :: Proxy (XNetNodeRef))
                                 ∘ constrainType Edge (Proxy :: Proxy (XNetEdgeRef e))
                                 ∘ flip Graph.execT net
{-# INLINE   buildNetworkM #-}
{-# INLINE rebuildNetworkM #-}


--newtype TargetGenRef2 r x t a = TargetGenRef2 (PhantomLayer (Ref (Targetting r (x a)) Int) t a) deriving (Show)



-----------------------
-- === TargetRef === --
-----------------------

newtype TargetRef r t a = TargetRef (PhantomLayer (Ref (Targetting r (t a)) Int) t a) deriving (Show)

type instance RefOf  (TargetRef r t a) = RefOf (Unwrapped (TargetRef r t a))
instance      HasRef (TargetRef r t a) where ref = wrapped' ∘ ref



-- === Instances === --


-- Wrappers
type instance Unlayered (XNetNodeRef n) = Unwrapped (XNetNodeRef n)
instance      Rewrapped (XNetNodeRef n) (XNetNodeRef n')
instance      Wrapped   (XNetNodeRef n) where
    type      Unwrapped (XNetNodeRef n) = (NodeRefPfx n) (n (EdgeRefPfx n))
    _Wrapped' = iso (\(XNetNodeRef a) -> a) XNetNodeRef

instance Monad m => LayerConstructor m (XNetNodeRef n) where constructLayer = return ∘ XNetNodeRef

type instance RefOf  (XNetNodeRef n) = RefOf (Unwrapped (XNetNodeRef n))
instance      HasRef (XNetNodeRef n) where ref = wrapped' ∘ ref


type instance Unlayered (XNetEdgeRef n) = Unwrapped (XNetEdgeRef n)
instance      Rewrapped (XNetEdgeRef n) (XNetEdgeRef n')
instance      Wrapped   (XNetEdgeRef n) where
    type      Unwrapped (XNetEdgeRef n) = (EdgeRefPfx n) (n (EdgeRefPfx n))
    _Wrapped' = iso (\(XNetEdgeRef a) -> a) XNetEdgeRef

instance Monad m => LayerConstructor m (XNetEdgeRef n) where constructLayer = return ∘ XNetEdgeRef

type instance RefOf  (XNetEdgeRef n) = RefOf (Unwrapped (XNetEdgeRef n))
instance      HasRef (XNetEdgeRef n) where ref = wrapped' ∘ ref



type instance Unlayered (NodeRefPfx n a) = Unwrapped (NodeRefPfx n a)
instance      Rewrapped (NodeRefPfx n a) (NodeRefPfx n' a')
instance      Wrapped   (NodeRefPfx n a) where
    type      Unwrapped (NodeRefPfx n a) = TargetGenRef2 Node (NetCover n a) (NetCover n) a
    _Wrapped' = iso (\(NodeRefPfx a) -> a) NodeRefPfx

instance Monad m => LayerConstructor m (NodeRefPfx n a) where constructLayer = return ∘ NodeRefPfx

type instance RefOf  (NodeRefPfx n a) = RefOf (Unwrapped (NodeRefPfx n a))
instance      HasRef (NodeRefPfx n a) where ref = wrapped' ∘ ref



type instance Unlayered (EdgeRefPfx n a) = Unwrapped (EdgeRefPfx n a)
instance      Rewrapped (EdgeRefPfx n a) (EdgeRefPfx n' a')
instance      Wrapped   (EdgeRefPfx n a) where
    type      Unwrapped (EdgeRefPfx n a) = TargetGenRef2 Edge (NetArcT  n  ) (NetCover n) a
    _Wrapped' = iso (\(EdgeRefPfx a) -> a) EdgeRefPfx

instance Monad m => LayerConstructor m (EdgeRefPfx n a) where constructLayer = return ∘ EdgeRefPfx

type instance RefOf  (EdgeRefPfx n a) = RefOf (Unwrapped (EdgeRefPfx n a))
instance      HasRef (EdgeRefPfx n a) where ref = wrapped' ∘ ref



-- Wrappers
type instance Unlayered (TargetRef r t a) = Unwrapped (TargetRef r t a)
instance      Rewrapped (TargetRef r t a) (TargetRef r' t' a')
instance      Wrapped   (TargetRef r t a) where
    type      Unwrapped (TargetRef r t a) = PhantomLayer (Ref (Targetting r (t a)) Int) t a
    _Wrapped' = iso (\(TargetRef a) -> a) TargetRef

-- Layers
instance Monad m => LayerConstructor m (TargetRef r t a) where constructLayer = return ∘ TargetRef


---------------------------------

newtype TargetGenRef r x t a = TargetGenRef (PhantomLayer (Ref (Targetting r x) Int) t a) deriving (Show)


type instance RefOf  (TargetGenRef r x t a) = RefOf (Unwrapped (TargetGenRef r x t a))
instance      HasRef (TargetGenRef r x t a) where ref = wrapped' ∘ ref

-- === Instances === --

-- Wrappers
type instance Unlayered (TargetGenRef r x t a) = Unwrapped (TargetGenRef r x t a)
instance      Rewrapped (TargetGenRef r x t a) (TargetGenRef r' x' t' a')
instance      Wrapped   (TargetGenRef r x t a) where
    type      Unwrapped (TargetGenRef r x t a) = PhantomLayer (Ref (Targetting r x) Int) t a
    _Wrapped' = iso (\(TargetGenRef a) -> a) TargetGenRef

-- Layers
instance Monad m => LayerConstructor m (TargetGenRef r x t a) where constructLayer = return ∘ TargetGenRef


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
-- ========================== --
-- === AST building utils === --
-- ========================== --

star :: Lit t
star = cons Star

star' :: (CoverConstructorFix m a, Builder Node a m, Uncovered a ~ Lit t) => m a
star' = registerNode =<< constructCoverFix star


starx :: Static Draft t
starx = cons star

starx' :: (CoverConstructorFix m a, Builder Node a m, Uncovered a ~ Static Draft t) => m a
starx' = registerNode =<< constructCoverFix starx

starx'2 :: (CoverConstructorFix m a, Builder Node a m, Uncovered a ~ Lit t) => m a
starx'2 = registerNode =<< constructCoverFix star

--starx2' :: (CoverConstructorFix m a, Builder Node a m, Uncovered a ~ Static Draft t, a ~ XNetNodeRef (Static Draft)) => m a
--starx2' :: (CoverConstructorFix m a, a ~ XNetNodeRef (Static Draft)) => m a



unifyx :: t (Static Draft t) -> t (Static Draft t) -> Static Draft t
unifyx a b = cons $ Unify a b

unifyx2 :: (CoverConstructorFix m a, Builder Node a m, Uncovered a ~ Static Draft t) => t (Static Draft t) -> t (Static Draft t) -> m a
unifyx2 a b = registerNode =<< constructCoverFix (unifyx a b)
 
unifyx' a b = mdo
    ca  <- connection a out
    cb  <- connection b out
    out <- registerNode =<< constructCoverFix (unifyx ca cb)
    return out
-------------------------------------------------------------------------------------------------------------------
-- TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST --
-------------------------------------------------------------------------------------------------------------------

newtype IDT a = IDT a deriving (Show, Functor, Traversable, Foldable)


-- TODO[WD]: Extend polymorphism, we can do exactly the same way as with AST elements
--           returning `m a`, where `a` can be either a connection in the graph

class EdgeBuilder src tgt m e  where edge :: src -> tgt -> m e

instance (MonadGraphBuilder n e m, Convertible (Arc (RefOf src) (RefOf tgt)) e, HaveRef '[src,tgt], a ~ Int) => EdgeBuilder src tgt m (Ref r a) where
    edge src tgt = Ref <$> Graph.modify (edges $ swap ∘ ixed add (convert $ arc src tgt))

instance (EdgeBuilder src tgt m (Ref (Targetting r (t a)) Int), Functor m) => EdgeBuilder src tgt m (TargetRef r t a) where edge = (TargetRef ∘ PhantomLayer) <∘∘> edge
instance (EdgeBuilder src tgt m (Unwrapped (Unwrapped (EdgeRefPfx n a))), Functor m) => EdgeBuilder src tgt m (EdgeRefPfx n a) where edge = (EdgeRefPfx ∘ PhantomLayer) <∘∘> edge
instance (EdgeBuilder src tgt m (Unwrapped (XNetEdgeRef n)), Functor m) => EdgeBuilder src tgt m (XNetEdgeRef n) where edge = XNetEdgeRef <∘∘> edge

instance (Convertible src src', Convertible tgt tgt') => Convertible (Arc src tgt) (Arc src' tgt') where convert = bimap convert convert


instance Convertible a a' => Convertible (Ref r a) (Ref r' a') where convert (Ref a) = Ref $ convert a
--instance 


connection :: (EdgeBuilder src tgt m e, Builder Edge e m) => src -> tgt -> m e
connection src tgt = registerEdge =<< edge src tgt

connection2 :: (EdgeBuilder src tgt m e) => src -> tgt -> m e
connection2 src tgt = edge src tgt



instance (Castable src src', Castable tgt tgt') => Castable (Arc src tgt) (Arc src' tgt') where cast = bimap cast cast

instance Castable (Ref r a) (Unwrapped (Unwrapped (Unwrapped (XNetEdgeRef n)))) => Castable (Ref r a) (XNetEdgeRef n) where cast = wrap' ∘ wrap' ∘ wrap' ∘ cast
instance Castable (Ref r a) (Unwrapped (Unwrapped (Unwrapped (XNetNodeRef n)))) => Castable (Ref r a) (XNetNodeRef n) where cast = wrap' ∘ wrap' ∘ wrap' ∘ cast

instance Castable (Ref r a) (Ref r' a) where cast = rewrap

type family XRef t (n :: (* -> *) -> *) :: *

type instance XRef Node n = XNetNodeRef n
type instance XRef Edge n = XNetEdgeRef n


instance (a' ~ n (EdgeRefPfx n), n ~ n') => Convertible (XNetEdgeRef n) (EdgeRefPfx n' a') where convert = unwrap'

main :: IO ()
main = do

        --s = star
    --g <- flip evalStateT (0 :: Int) $ flip Graph.execT (def :: Network) $ do
       -- $ constrainCoverType (Proxy :: Proxy (Ref' Node (Attached' String Cover)))

    g <- buildNetworkM $ do
        --x <- constructCoverFix star :: _ (TargetRef Node (Attached' String Cover) (Lit (TargetRef Edge  (Attached' String Cover))))


        s1 <- starx' :: _
        s2 <- starx'
        s3 <- starx'2 :: _


        c <- connection s1 s2 :: _ (XNetEdgeRef (Static Draft))

        --let tn = s1 :: XRef Node (Static Draft)

        --    tc = c  :: XRef Edge (Static Draft)

        --print s2
        --print c

        s1_v <- readRef s1

        c_v <- readRef c

        print (c_v :: _)

        uu <- unifyx2 (convert c) (convert c)

        --u <- unifyx' s1 s2




        --s1' <- readRef s1
        --c'  <- readRef c

        --print s1'
        --print c'

        return ()

        --s' = s & coated %~ unwrap' ∘ unwrap'

    print $ (g :: Network)
    --print $ s'
    --print $ uncoat s
    ----let x = 
    ----let v  = star :: Lit Int IDT
    --let v  = cons Star :: Lit IDT
    --    v' = cons v    :: Dynamic Draft IDT
    --    t1 = cons' (IDT v') [] :: Dynamic Draft IDT
    ----let v  = checkedVariantCons Star :: Ok (Static Draft IDT)
    ----let v  = checkedVariantCons (1 :: Int) :: Ok (Lit IDT)
    --    --l  = checkedVariantCons v :: Static Thunk Int IDT
    --    --l2 = checkedVariantCons l  :: Dynamic Val Int IDT
    --    --l2 = checkedVariantCons l  :: Dynamic Thunk Int IDT

    --print v
    --print v'
    --print t1



    ----print l
    ----print l2

    --print $ caseTest t1 $ do
    --    --match $ \Star    -> "star!"
    --    --dynamic $ \s -> "its dynamic! :O"
    --    static $ \s -> "it is static!  :O"
    --    match  $ \(Cons _ _) -> "its cons ..."
    --    --match  $ \(Lit l) -> caseTest l $ do
    --    --    match $ \Star -> "its star!"
    --    match $ \ANY     -> "something else"
    



    return ()


-- time  take  -  description                    FIXME
-----------------------------------------------------
--           [+] readRef dla Edge               
-- 0:35  30  [ ] automatic constructed connection type
--       30  [ ] types
--       30  [ ] destructors
--       30  [ ] successors
--       30  [ ] predecessors
--       30  [ ] attach accessors
--       30  [ ] nice connect / reconnect
--       30  [ ] term construction methods
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