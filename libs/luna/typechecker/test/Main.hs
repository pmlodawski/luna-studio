{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

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

-------------------------------------------------------------------------------------------------------------------
-- TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST --
-------------------------------------------------------------------------------------------------------------------

newtype IDT a = IDT a deriving (Show, Functor, Traversable, Foldable)

--star' :: ASTRecord '[] '[] IDT
--star' = checkedVariantCons $ Star +> 5

--star :: Lit (Labeled String (Labeled Int Cover))
--star :: Lit (Labeled String (Labeled Int) Cover)
data Attached d t = Attached d t deriving (Show)

newtype Layer l t a = Layer (l (t a)) deriving (Show)

data Entity = Node
            | Edge
            deriving (Show)


data Ref (t :: Entity) a = Ref a deriving (Show, Functor, Traversable, Foldable)

newtype Typed a t = Typed a deriving (Show, Functor, Traversable, Foldable)

instance Rewrapped (Typed a t) (Typed a' t')
instance Wrapped   (Typed a t) where
    type Unwrapped (Typed a t) = a
    _Wrapped' = iso (\(Typed a) -> a) Typed ; {-# INLINE _Wrapped' #-}


-------------------
-- === Edges === --
-------------------

data Arrow       tgt = Arrow                (Ref 'Node tgt) deriving (Show, Functor, Traversable, Foldable)
data Arc     src tgt = Arc   (Ref Node src) (Ref 'Node tgt) deriving (Show, Functor, Traversable, Foldable)
type HomoArc t       = Arc t t

class EdgeCons src tgt edge where consEdge :: src -> tgt -> edge


-- === Construction === --

-- Arc
type ArcCons src tgt srcRef tgtRef = ( HaveRef '[src,tgt]
                                     , RefOf src ~ Ref 'Node srcRef
                                     , RefOf tgt ~ Ref 'Node tgtRef
                                     )

type HomoArcCons t tRef = ArcCons t t tRef tRef

instance   ArcCons     src tgt srcRef tgtRef => EdgeCons src tgt (Arc srcRef tgtRef) where consEdge = arc ; {-# INLINE consEdge #-}
arc     :: ArcCons     src tgt srcRef tgtRef => src -> tgt -> Arc srcRef tgtRef
homoArc :: HomoArcCons t       tRef          => t   -> t   -> HomoArc tRef
arc src tgt = Arc (src ^. ref) (tgt ^. ref)
homoArc     = arc

-- Arrow
type ArrowCons tgt tgtRef = ( HasRef tgt
                            , RefOf tgt ~ Ref 'Node tgtRef
                            )

instance ArrowCons tgt tgtRef => EdgeCons src tgt (Arrow tgtRef) where consEdge = const arrow ; {-# INLINE consEdge #-}
arrow :: ArrowCons tgt tgtRef => tgt -> Arrow tgtRef
arrow tgt = Arrow (tgt ^. ref)









instance Coated Cover        where coated = lens (\(Cover a) -> a) (const Cover)
instance Coated (Attached d) where coated = lens (\(Attached _ t) -> t) (\(Attached d _) t -> Attached d t)

instance (Coated l, Coated t) => Coated (Layer l t) where coated = wrapped ∘ coated ∘ coated

type instance        Unlayered (Layer l t a) = t a
instance Coated l => Layered   (Layer l t a) where layered = wrapped' ∘ coated ; {-# INLINE layered #-}

instance Rewrapped (Layer l t a) (Layer l' t' a')
instance Wrapped   (Layer l t a) where
    type Unwrapped (Layer l t a) = l (t a)
    _Wrapped' = iso (\(Layer a) -> a) Layer
    {-# INLINE _Wrapped' #-}



--instance (CoatConstructor m l, Functor m) => LayerConstructor m (Layer l t a) where constructLayer = Layer <∘> constructCoat
instance (Functor m, CoatConstructor (t a) m l) => LayerConstructor m (Layer l t a) where constructLayer = Layer <∘> constructCoat

instance {-# OVERLAPPABLE #-} (Default d, Monad m) => CoatConstructor a m (Attached d) where constructCoat = return ∘ Attached def


--foo :: (Monad m, MonadGraphBuilder (Layer (Attached String) Cover (ASTRecord '[] LitVariants (Layer (Ref :< Attached String) Cover) Data)) e m)
--    => m (Layer Ref (Layer (Attached String) Cover) (Lit (Ref :< (Attached String) :< Cover)))

--foo :: GraphBuilder (Layer (Attached String) Cover Data) Int (Layer Ref (Layer (Attached String) Cover) (Lit (Ref :< (Attached String) :< Cover)))
--foo :: GraphBuilder (Layer (Attached String) Cover Data) Int 
--       (      Layer Ref (Layer (Attached String) Cover) 
--        (Lit (Layer Ref (Layer (Attached String) Cover)))   
--       )
--foo :: (CoverConstructor m a, Uncovered a ~ Lit ((Ref :< Attached String) :< Cover)) => m a
--foo2 :: _ => m (t (a t))
foo2 = constructCover' star2

foo = constructCover' star


foox = constructCoverFix starx

--Ref a 

--ref :: Lens a (Ref (RefData a))


--class IsEdge where
--    asEdge :: 


--Ref Node Int -> Ref Node Int -> Ref Edge Int

--Ref Node Int -> Ref Node Int -> Arc Int
--star :: Lit (Ref' Edge  (Attached' String Cover)  )


--constructCover' :: (CoverConstructorFix m (t (a t)), Uncovered (t (a t)) ~ a t) => (a t) -> m (t (a t))
constructCover' :: (Uncovered (t (a (Ref' 'Edge t))) ~ a (Ref' 'Edge t), MonadFix m, CoverConstructorFix m (t (a (Ref' 'Edge t))), Wrapped (Unwrapped (a (Ref' 'Edge t))), Wrapped (a (Ref' 'Edge t)), Coated t, MonadGraphBuilder (t (Unwrapped (Unwrapped (a (Ref' 'Edge t))))) e m)
                => (a (Ref' Edge t)) -> m (Ref' Node t (a (Ref' Edge t)))
constructCover' = constructCoverFix

--foo :: (Monad m, CoatConstructor m Ref) => m (Layer Ref (Layer (Attached String) Cover) (Lit (Ref :< (Attached String) :< Cover)))
--foo = constructCover star


--instance (MonadGraphBuilder n e m, Coated l, Wrapped ast, n ~ l (Unwrapped ast)) => CoatConstructor (l ast) m Ref where
--    constructCoat ast = do
--        g <- Graph.get
--        let generalizedAST = ast & coated %~ unwrap'
--            (n', idx)      = ixed add generalizedAST (g ^. nodes)
--        Graph.put (g & nodes .~ n')
--        return $ Ref idx

instance (MonadGraphBuilder n e m, Coated l, Wrapped ast, Wrapped (Unwrapped ast), n ~ l (Unwrapped (Unwrapped ast)), a ~ Int)
      => CoatConstructor (l ast) m (Typed (Ref 'Node a)) where
    constructCoat ast = do
        idx <- Graph.modify $ \g -> let generalizedAST = ast & coated %~ unwrap' ∘ unwrap'
                                        (n', idx)      = ixed add generalizedAST (g ^. nodes)
                                    in  (g & nodes .~ n', idx)
        return $ Typed $ Ref idx
    {-# INLINE constructCoat #-}
            
                          
type Network = Graph (Layer (Attached String) Cover Data) Int  


        --Graph.put (g & nodes .~ n')
        --return $ Ref idx
--test :: _ => _
--test a (v :: Graph.AutoVector y) = ixed add a v


--instance Covered (Layer l t a) where
--    covered = lens (\(Layer ))

--infixl 9 +>
--type l +> d = Attached d l

infixl 9 :<
type l :< t = Layer l t

--star :: Lit (Cover +> Int +> String)
--star :: Lit (Attached String (Attached Int Cover))
--star :: Lit (Layer Ref (Layer (Attached String) Cover))
--star :: Lit (Ref :< (Attached String) :< Cover)
--star :: Lit (Layer Ref (Layer (Attached String) Cover))
--star = cons Star

type Ref'      t = Layer (Typed (Ref t Int))
type Attached' t = Layer (Attached t)

newtype GRef r t a = GRef (Layer (Typed (Ref r Int)) t a)

--star :: Lit (Layer (Typed (Ref Edge Int)) t)
--star :: Lit (Layer (Typed (Ref Edge Int))   (Layer (Attached String) Cover)  )
star :: Lit (Ref' Edge  (Attached' String Cover)  )
star = cons Star

star2 :: Lit t
star2 = cons Star

starx :: Lit (GRef Edge  (Attached' String Cover)  )
starx = cons Star

--star :: Lit t
--star = cons Star

cons' :: SmartCons (Cons n t) b => n -> [t] -> b
cons' = cons ∘∘ Cons

caseTest = __case__ "tc-test" "test/Main.hs" 0
{-# INLINE caseTest #-}

data Test a b = Test !a !b  deriving (Show)

type family RefOf a
class HasRef a where ref :: Lens' a (RefOf a)
type family HaveRef lst :: Constraint where 
    HaveRef '[]       = ()
    HaveRef (a ': as) = (HasRef a, HaveRef as)

type instance RefOf (Ref t a) = Ref t a
instance HasRef (Ref t a) where ref = id

type instance RefOf (Layer l t a) = RefOf (Unwrapped (Layer l t a))
type instance RefOf (Typed a t)   = RefOf a

instance HasRef (Unwrapped (Layer l t a)) => HasRef (Layer l t a) where ref = wrapped' ∘ ref
instance HasRef (Unwrapped (Typed a t))   => HasRef (Typed a t)   where ref = wrapped' ∘ ref

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


buildNetwork = rebuildNetwork def
rebuildNetwork (net :: Network) = constrainCoverType (Proxy :: Proxy (Ref' 'Node k)) -- ta linijka wystarcza do inferencji typow (!)
                                ∘ flip Graph.execT net

--build


foo' = do
    out <- constructCoverFix star2
    register (Proxy :: Proxy AST) out
    return out

main :: IO ()
main = do

        --s = star
    --g <- flip evalStateT (0 :: Int) $ flip Graph.execT (def :: Network) $ do
       -- $ constrainCoverType (Proxy :: Proxy (Ref' 'Node (Attached' String Cover)))

    g <- buildNetwork $ do             -- mowi ona TC ze powinien unifikowac potrzebne typy wkladane do grafu z tymi generowanymi

        s1 <- foo'
        s2 <- foo'

                        --zastanowic sie nad tym czy nie ma lepszego rozwiazani dla problemu wyzej (inferowania typu coverow)
                        --dorobic prawdziwe newtype nad Ref'
                        --Network powinien przechowywac Arc dla krawedzi, tak samo jak teraz przechowuje Data dla wierzcholkow
                        --typujemy krawedzie dokladniej przy odczycie

        print $ arc s1 s2
        --print $ (s1 ^. ref)
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