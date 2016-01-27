{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures #-}

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

newtype IDT a = IDT a deriving (Show, Functor, Traversable, Foldable)

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

newtype Targetting a t = Targetting a   deriving (Show, Functor, Traversable, Foldable)
data    Attached   d t = Attached   d t deriving (Show, Functor, Traversable, Foldable)


-- === Instances === --

-- Functors

instance Bifunctor Attached where bimap f g (Attached d t) = Attached (f d) (g t)

-- Wrappers

instance Rewrapped (Targetting a t) (Targetting a' t')
instance Wrapped   (Targetting a t) where
    type Unwrapped (Targetting a t) = a
    _Wrapped' = iso (\(Targetting a) -> a) Targetting ; {-# INLINE _Wrapped' #-}

-- Targets

type instance Target (Targetting a t) = t

-- Conversions

instance (Castable d d', Castable a a') => Castable (Attached d a) (Attached d' a') where cast = bimap cast cast



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
            ast = cast d
        return ast

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








instance Coated Cover        where coated = lens (\(Cover a) -> a) (const Cover)
instance Coated (Attached d) where coated = lens (\(Attached _ t) -> t) (\(Attached d _) t -> Attached d t)





--instance (CoatConstructor m l, Functor m) => LayerConstructor m (Layer l t a) where constructLayer = Layer <∘> constructCoat
instance (Functor m, CoatConstructor (t a) m l)           => LayerConstructor m (Layer        l t a) where constructLayer = Layer        <∘> constructCoat
instance (Functor m, Destructed l ~ t a, Constructor m l) => LayerConstructor m (PhantomLayer l t a) where constructLayer = PhantomLayer <∘> construct

instance {-# OVERLAPPABLE #-} (Default d, Monad m) => CoatConstructor a m (Attached d) where constructCoat = return ∘ Attached def





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

--class IsEdge e 
--connection src tgt = do 
--    g <- Graph.get

--    return ()


--type family Derefd a
--class Deref a where deref :: a -> Derefd a

--instance Deref (Layer l t a) where deref = 

--data AST = AST  deriving (Show)

--class TypeResolver t m a | t m -> a where resolveType :: Proxy t -> a -> m a

--tst :: _ => _
--tst = do
--    a1 <- resolveType (Proxy :: Proxy AST) =<< constructCover star
--    a2 <- resolveType (Proxy :: Proxy AST) =<< constructCover star
--    a3 <- resolveType (Proxy :: Proxy AST) =<< constructCover star
--    return ()

--instance TypeResolver AST (Graph.GraphBuilderT (t a) e m) 



newtype TypeConstraint t tp m a = TypeConstraint (m a) 

constrainType :: Proxy t -> Proxy tp -> TypeConstraint t tp m a -> m a
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

--mfix :: (a -> m a) -> m a

--instance Applicative (TypeConstraint)

--t (a p)

instance {-# OVERLAPPABLE #-} (Monad m, Builder t a m, tp ~ a) => Builder t a (TypeConstraint t  tp m) where register = lift ∘∘ register
instance {-# OVERLAPPABLE #-} (Monad m, Builder t a m)         => Builder t a (TypeConstraint t' tp m) where register = lift ∘∘ register

constrainNodeType = constrainType (Proxy :: Proxy Node)
constrainEdgeType = constrainType (Proxy :: Proxy Edge)
constrainCoverType (Proxy :: Proxy tp) = constrainNodeType (Proxy :: Proxy (tp k))



------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------
-- =============== --
-- === Network === --
-- =============== --

--type NetRef     tp tgt = Ref tp tgt Int
--type NetFreeRef tp     = NetRef tp 'Nothing

type Network        = Graph (NetNodeWrapper Data) NetArc
type NetNodeWrapper = Attached' String Cover
type NetArc         = Arc (Ref Node Int) (Ref Node Int)
type NetRef r       = Ref r Int

type Attached' t = Layer (Attached t)


-- === Construction === ---

buildNetwork = rebuildNetwork def
rebuildNetwork (net :: Network) = constrainNodeType (Proxy :: Proxy (TargetRef Node NetNodeWrapper k))
                                ∘ constrainEdgeType (Proxy :: Proxy (NetRef NetArc))
                                ∘ flip Graph.execT net



-----------------------
-- === TargetRef === --
-----------------------

newtype TargetRef r t a = TargetRef (PhantomLayer (Ref (Targetting r (t a)) Int) t a)

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
instance                  Builder t a IO                          where register _ _ = return ()

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



-------------------------------------------------------------------------------------------------------------------
-- TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST --
-------------------------------------------------------------------------------------------------------------------


-- TODO[WD]: Extend polymorphism, we can do exactly the same way as with AST elements
--           returning `m a`, where `a` can be either a connection in the graph

class EdgeBuilder src tgt m e where edge :: src -> tgt -> m e

instance (MonadGraphBuilder n e m, Convertible (Arc (RefOf src) (RefOf tgt)) e, HaveRef '[src,tgt], a ~ Int) => EdgeBuilder src tgt m (Ref r a) where
    edge src tgt = Ref <$> Graph.modify (edges $ swap ∘ ixed add (convert $ arc src tgt))

instance (Convertible src src', Convertible tgt tgt') => Convertible (Arc src tgt) (Arc src' tgt') where convert = bimap convert convert
instance Convertible a a' => Convertible (Ref r a) (Ref r' a') where convert (Ref a) = Ref $ convert a

connection :: (EdgeBuilder src tgt m e, Builder Edge e m) => src -> tgt -> m e
connection src tgt = registerEdge =<< edge src tgt

main :: IO ()
main = do

        --s = star
    --g <- flip evalStateT (0 :: Int) $ flip Graph.execT (def :: Network) $ do
       -- $ constrainCoverType (Proxy :: Proxy (Ref' Node (Attached' String Cover)))

    g <- buildNetwork $ do
        --x <- constructCoverFix star :: _ (TargetRef Node (Attached' String Cover) (Lit (TargetRef Edge  (Attached' String Cover))))

        s1 <- star'

        s2 <- star'

        a <- readRef s1

        c <- connection s1 s2
        c <- connection s1 s2
        c <- connection s1 s2
        c <- connection s1 s2
        c <- connection s1 s2
        c <- connection s1 s2
        c <- connection s1 s2
        c <- connection s1 s2
        c <- connection s1 s2

        print c

        return ()

        --s' = s & coated %~ unwrap' ∘ unwrap'

    print $ g
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