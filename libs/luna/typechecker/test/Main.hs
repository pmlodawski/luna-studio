{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prologue hiding (simple, empty, Indexable, Simple, cons, lookup, index, children, Cons, Ixed, Repr, repr, minBound, maxBound, (#), assert, Index)

import Data.Record




import Luna.Syntax.AST.Term
import Luna.Syntax.Model.Layer.Labeled


import Data.Layer.Cover
import Data.Coat
import Data.Construction

import Control.Monad.Identity
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

data Ref a = Ref Int



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

foo :: GraphBuilder (Layer (Attached String) Cover Data) Int (Layer Ref (Layer (Attached String) Cover) (Lit (Ref :< (Attached String) :< Cover)))
foo = constructCover star

--foo :: (Monad m, CoatConstructor m Ref) => m (Layer Ref (Layer (Attached String) Cover) (Lit (Ref :< (Attached String) :< Cover)))
--foo = constructCover star


--instance (MonadGraphBuilder n e m, Coated l, Wrapped ast, n ~ l (Unwrapped ast)) => CoatConstructor (l ast) m Ref where
--    constructCoat ast = do
--        g <- Graph.get
--        let generalizedAST = ast & coated %~ unwrap'
--            (n', idx)      = ixed add generalizedAST (g ^. nodes)
--        Graph.put (g & nodes .~ n')
--        return $ Ref idx

instance (MonadGraphBuilder n e m, Coated l, Wrapped ast, Wrapped (Unwrapped ast), n ~ l (Unwrapped (Unwrapped ast)))
      => CoatConstructor (l ast) m Ref where
    constructCoat ast = fmap Ref $ Graph.modify $ \g -> let generalizedAST = ast & coated %~ unwrap' ∘ unwrap'
                                                            (n', idx)      = ixed add generalizedAST (g ^. nodes)
                                                        in  (g & nodes .~ n', idx)
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
star :: Lit (Ref :< (Attached String) :< Cover)
star = cons Star

cons' :: SmartCons (Cons n t) b => n -> [t] -> b
cons' = cons ∘∘ Cons

caseTest = __case__ "tc-test" "test/Main.hs" 0
{-# INLINE caseTest #-}

data Test a b = Test !a !b  deriving (Show)



main = do

    let 
        --s = star
        s = Graph.exec foo (def :: Network)
        --s' = s & coated %~ unwrap' ∘ unwrap'

    print $ s
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