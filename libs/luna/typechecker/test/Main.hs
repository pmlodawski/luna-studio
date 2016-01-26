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

instance (Coated l, Coated t) => Coated (Layer l t) where coated = wrapped ∘ coated ∘ coated
type instance        Unlayered (Layer l t a) = t a
instance Coated l => Layered   (Layer l t a) where layered = wrapped' ∘ coated ; {-# INLINE layered #-}
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

newtype Ptr      a t = Ptr      a   deriving (Show, Functor, Traversable, Foldable)
data    Attached d t = Attached d t deriving (Show, Functor, Traversable, Foldable)


-- === Instances === --

-- Functors

instance Bifunctor Attached where bimap f g (Attached d t) = Attached (f d) (g t)

-- Wrappers

instance Rewrapped (Ptr a t) (Ptr a' t')
instance Wrapped   (Ptr a t) where
    type Unwrapped (Ptr a t) = a
    _Wrapped' = iso (\(Ptr a) -> a) Ptr ; {-# INLINE _Wrapped' #-}

-- Targets

type instance Target (Ptr a t) = t

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
type instance Destructed (Ref (Ptr r t) a) = t

type instance RefOf  (Ref t a) = Ref t a
instance      HasRef (Ref t a) where ref = id

-- Wrappers

instance Rewrapped (Ref r a) (Ref r' a')
instance Wrapped   (Ref r a) where
    type Unwrapped (Ref r a) = a
    _Wrapped' = iso (\(Ref a) -> a) Ref


instance (a ~ Int, MonadGraphBuilder n e m, Castable n t)
      => RefGetter (Ref (Ptr Node t) a) m where
    getRef ref = do
        g <- Graph.get
        let d   = index_ (unwrap' ref) $ g ^. nodes
            ast = cast d
        return ast

instance (a ~ Int, MonadGraphBuilder n e m, Castable t n)
      => Constructor m (Ref (Ptr Node t) a) where
    construct ast = do
        idx <- Graph.modify $ \g -> let (n', idx) = ixed add (cast ast) (g ^. nodes)
                                    in  (g & nodes .~ n', idx)
        return $ Ref idx

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



type instance RefOf (Ptr a t)   = RefOf a

instance HasRef (Unwrapped (Ptr a t))   => HasRef (Ptr a t)   where ref = wrapped' ∘ ref

--class IsEdge e 
--connection src tgt = do 
--    g <- Graph.get

--    return ()


--type family Derefd a
--class Deref a where deref :: a -> Derefd a

--instance Deref (Layer l t a) where deref = 

data AST = AST  deriving (Show)

--class TypeResolver t m a | t m -> a where resolveType :: Proxy t -> a -> m a

--tst :: _ => _
--tst = do
--    a1 <- resolveType (Proxy :: Proxy AST) =<< constructCover star
--    a2 <- resolveType (Proxy :: Proxy AST) =<< constructCover star
--    a3 <- resolveType (Proxy :: Proxy AST) =<< constructCover star
--    return ()

--instance TypeResolver AST (Graph.GraphBuilderT (t a) e m) 

class Monad m => Builder t a m where register :: Proxy t -> a -> m ()

instance Builder t a m => Builder t a (Graph.GraphBuilderT n e m) where register = lift ∘∘ register
instance Builder t a m => Builder t a (StateT                s m) where register = lift ∘∘ register
instance                  Builder t a IO                          where register _ _ = return ()

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

instance (Monad m, tp ~ a, Builder t a m) => Builder t a (TypeConstraint t tp m) where register = lift ∘∘ register

constrainASTType = constrainType (Proxy :: Proxy AST)
constrainCoverType (Proxy :: Proxy tp) = constrainASTType (Proxy :: Proxy (tp k))



------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------
-- =============== --
-- === Network === --
-- =============== --

type NetworkWrapper = Attached' String Cover

type Network = Graph (NetworkWrapper Data) Int  

type Attached' t = Layer (Attached t)


-- === Construction === ---

buildNetwork = rebuildNetwork def
rebuildNetwork (net :: Network) = constrainASTType (Proxy :: Proxy (NetRef Node NetworkWrapper k)) -- ta linijka wystarcza do inferencji typow (!)
                                ∘ flip Graph.execT net                                             -- mowi ona TC ze powinien unifikowac potrzebne typy wkladane do grafu z tymi generowanymi



--------------------
-- === NetRef === --
--------------------

newtype NetRef r t a = NetRef (PhantomLayer (Ref (Ptr r (t a)) Int) t a)

type instance RefOf  (NetRef r t a) = RefOf (Unwrapped (NetRef r t a))
instance      HasRef (NetRef r t a) where ref = wrapped' ∘ ref


-- === Instances === --

-- Wrappers
type instance Unlayered (NetRef r t a) = Unwrapped (NetRef r t a)
instance      Rewrapped (NetRef r t a) (NetRef r' t' a')
instance      Wrapped   (NetRef r t a) where
    type      Unwrapped (NetRef r t a) = PhantomLayer (Ref (Ptr r (t a)) Int) t a
    _Wrapped' = iso (\(NetRef a) -> a) NetRef

-- Layers
instance Monad m => LayerConstructor m (NetRef r t a) where constructLayer = return ∘ NetRef


------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------
-- ======================== --
-- === AST construction === --
-- ======================== --

registerAST m = do 
    out <- m
    register (Proxy :: Proxy AST) out
    return out

star :: Lit t
star = cons Star


star' :: (CoverConstructorFix m a, Builder AST a m, Uncovered a ~ Lit t) => m a
star' = registerAST $ constructCoverFix star



-------------------------------------------------------------------------------------------------------------------
-- TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST --
-------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do

        --s = star
    --g <- flip evalStateT (0 :: Int) $ flip Graph.execT (def :: Network) $ do
       -- $ constrainCoverType (Proxy :: Proxy (Ref' Node (Attached' String Cover)))

    g <- buildNetwork $ do
        --x <- constructCoverFix star :: _ (NetRef Node (Attached' String Cover) (Lit (NetRef Edge  (Attached' String Cover))))

        s1 <- star'
        s2 <- star'

        a <- readRef s1

        print a

        print $ arc s1 (s2 :: _)
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