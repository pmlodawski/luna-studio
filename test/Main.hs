
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoOverloadedStrings #-}


module Main where

import Flowbox.Prelude hiding (simple, empty, Indexable, Simple, cons, lookup, index, Wrapped, children, Cons, Ixed)
--import Data.Repr

import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.Text.Lazy      as Text
import           Data.Text.Lazy      (Text)
import           GHC.Prim (Any)
import           GHC.Int
import           Unsafe.Coerce (unsafeCoerce)
import           Data.Convert
import qualified Data.IntMap.Lazy as IntMap
import           Data.IntMap.Lazy (IntMap)
import Data.Typeable       hiding (cast)
import qualified Control.Monad.State as State
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable ()
import Data.Maybe (fromJust)
import Data.Containers
import Data.Containers.Hetero
import Data.Containers.Resizable
import Data.Containers.Reusable
import System.Process
import qualified Data.Text.AutoBuilder as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Constraint
import Control.Error.Util (hush)
import Data.Convert.Errors (TypeMismatch (TypeMismatch))
import Data.Constraint.Void
import Data.Variants
import qualified Data.Variants as V
import Flowbox.System.Types hiding ((.:), insert)
import           Control.Monad.State.Generate (newState)
import Text.Read (readMaybe)
import           Luna.Syntax.Builder.Graph
import qualified Luna.Syntax.Builder.Graph as GraphBuilder
import           Luna.Syntax.Builder
import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Lit
import           Luna.Syntax.AST
import           Luna.Syntax.AST.Decl
import           Luna.Syntax.Name.Pool
import Control.Monad.Fix
import Data.Cata
import           Luna.Syntax.Builder.Star (MonadStarBuilder)
import           Luna.Syntax.Builder.Star (StarBuilder, StarBuilderT)
import qualified Luna.Syntax.Builder.Star as StarBuilder
import Control.Monad.Trans.Identity
import Luna.Diagnostic.AST (toGraphViz, display)
import Luna.Syntax.Layer.Typed
import Luna.Syntax.Layer.Labeled
import qualified Type.BaseType as BT
import Data.Containers.Interface

--import Data.Text.CodeBuilder.Builder
import Data.Text.CodeBuilder.Builder as CB
import           Data.Containers.Poly {- x -} hiding (append)

import Data.Vector.Dynamic as VD


-- === HomoBuilder ===

newtype HomoG (t :: * -> *) m a = HomoG { fromHomoG :: IdentityT m a } deriving (MonadFix, Monad, Functor, Applicative, MonadTrans)

runHomoBuilder :: HomoG t m a -> m a
runHomoBuilder = runIdentityT . fromHomoG


instance (MuBuilder a m t, t ~ t') => MuBuilder a (HomoG t m) t' where
    buildMu = lift . buildMu

-------------------------------------------------------------

        --nytst2 :: (Arc (Labeled Int (Typed Draft)), HomoGraph ArcPtr (Labeled Int (Typed Draft)))
        --nytst2 = flip runGraph def $ do
        --    s    <- genTopStar

        --    i1   <- int 1
        --    i2   <- blank
        --    plus <- i1 @. "+"
        --    sum  <- plus @$ [arg i1, arg i2]
        --    return s

--nytst3 :: Mu (Typed Draft)
--nytst3 = flip StarBuilder.eval Nothing $ runIdentityT $ do
--    v1 <- var "foo"
--    return v1

--nytst2f :: (Arc (Labeled Int (Typed Draft)), Function (HomoGraph ArcPtr (Labeled Int (Typed Draft))))
--nytst2f = flip runFunctionBuilder def $ do
--    v1 <- var "foo"
--    v2 <- var "bar"
--    s  <- star
--    a  <- v1 @. "x"
--    x  <- v1 @$ [arg v2]
--    y  <- x @. "y"
--    return v1

            --viewGraph = view graph <$> GraphBuilder.get
            --mapMGraph f = do
            --    g   <- viewGraph
            --    mapM f g

            --(s, gr) = nytst2

            --unifyLit = \case
            --    _ -> do
            --        (s :: Arc (Labeled Int (Typed Draft))) <- string "foo"
            --        return ()

            --data Constraint a = ConstraintType a a

            ----withType t = do
            --typed a t = StarBuilder.with (const $ Just t) a

        --pass2 :: ([Arc (Labeled Int (Typed Draft))], HomoGraph ArcPtr (Labeled Int (Typed Draft)))
        --pass2 = buildGraph (Just s) (BldrState [] gr) $ do
        --    g <- viewGraph
        --    let ptrs = indexes g :: [Int]
        --    unis <- fmap concat . flip mapM ptrs $ \ptr -> do
        --        g <- viewGraph
        --        let (Labeled l (Typed t ast)) = index ptr g
        --        out <- case' ast $ do
        --            match $ \case
        --                Int i -> do
        --                    (i :: _) <- string "Int"
        --                    s <- star `typed` i
        --                    u <- unify (Mu (Ref (Ptr ptr))) s
        --                    return [u]
        --                String s -> return []
        --            match $ \ANY -> return []
        --        return out
        --    return unis

        --(unis2, gr2) = pass2

        --class Union m a where
        --    union :: a -> a -> m ()

        --pass3 :: ((), HomoGraph ArcPtr (Labeled Int (Typed Draft)))
        --pass3 = buildGraph (Just s) (BldrState [] gr2) $ do
        --    BldrState x g <- GraphBuilder.get
        --    flip mapM unis2 $ \uni -> do
        --        let cptr                     = ptrIdx . fromRef . fromMu
        --            Labeled l (Typed t ast') = index (cptr uni) g
        --        case' ast' $ match $ \(Unify pa pb) -> do
        --            let ptra = cptr pa
        --                ptrb = cptr pb
        --                Labeled la (Typed ta asta) = index ptra g
        --                Labeled lb (Typed tb astb) = index ptrb g
        --            case' astb $ do
        --                match $ \Star -> do
        --                    let Labeled lta (Typed tta astta) = index (cptr ta) g
        --                    case' astta $ do
        --                        match $ \Star -> do
        --                            let g' = insert ptra (Labeled la (Typed tb asta)) g
        --                            GraphBuilder.put $ BldrState x g'
        --                            return ()
        --                    --string "fooooooo"

        --            return ()
        --        return ()
        --    return ()

        --gr3 = snd pass3

valCons :: Variant a (Val t) => a -> Val t
valCons = cons

--val = cons . valCons


-- TODO: tworzenie Val i Thunk, poprawic polimorficznosc w konstruktorach Variantow
-- jak zrobic castowanie pomiedzy Thunk i Val? Czy castowanie kiedykolwiek bedzie potrzebne? chyba nie!
-- nawet jesli nie to castowanie polimorficznych variantow nie dziala dobrze

--valx :: Val t
--valx :: forall t. Term t
--valx = cons $ (valCons (Int 5) :: Val t)

--gr2 :: ((), HomoGraph ArcPtr (Labeled Int (Typed Draft)))
--gr2 = buildGraph (Just s) (BldrState [] gr) $ do

    --let ptrs = indexes g :: [Int]
    --flip mapM ptrs $ \ptr -> do
    --    BldrState x g <- GraphBuilder.get
    --    let (Labeled l (Typed t ast)) = index ptr g
    --    i <- case' ast $ do
    --        match $ \case
    --            Int    i -> string "foo1"
    --            String s -> string "foo2"
    --        match $ \ANY -> return t

    --    BldrState x g <- GraphBuilder.get
    --    let out = Labeled l (Typed i ast)
    --        g'  = insert ptr out g
    --    GraphBuilder.put $ BldrState x g'
    --    return ()

    --g <- view graph <$> GraphBuilder.get
    --withGraphM_ $ \g -> flip mapM g $ \(Labeled l (Typed t ast)) -> do
    --    xx <- case' ast $ do
    --        match $ \case
    --            Int    i -> string "foo1"
    --            String s -> string "foo2"
    --        match $ \ANY -> string "foo3"
    --    i <- string "xxx"
    --    return (Labeled l (Typed t ast))
    --return ()

--withGraphM :: MonadGraphBuilder g m => (g -> m (g, a)) -> m a


--gr3 :: (Arc Term, HomoGraph ArcPtr Term)
--gr3 = flip runGraph def $ do
--    genTopStar
--    i1   <- int 1
--    flip mapM gr $ \(Labeled l t) -> do
--        --case' t $ do
--        --    match $ \ANY -> return ()
--        i <- int 7
--        return ()
--    return i1

--gr2 = flip mapM gr $ \(Labeled l t) -> do
--    v <- int 7
--    return (Labeled l t)

--type AutoVector a = Reusable (Resizable Duplicate (Vector a))
--type AutoVector a = Reusable (Resizable Duplicate (Vector a))

    --main = do
    --    --putStrLn $ repr y
    --        --print $ repr $ nytst2
    --    --putStrLn $ repr $ nytst3
    --    --print c'
    --    --print $ take 1000 names

    --    --let gv = toGraphViz gr3
    --    --print   gv
    --    --display gv

    --    VD.tst

    --    --V.test
    --    --BT.test
    --    --print tstc
    --    let xa  = alloc 4 :: AutoVector Int
    --        v = mempty :: Vector Int
    --        --i  = 600000
    --        --xb = add (1::Int) xa
    --        --xc = add (1::Int) xb
    --        --xd = add (1::Int) xc
    --        --xe = add (1::Int) xd
    --        --xf = add (1::Int) xe
    --        --xg = unsafeErase 2 xf
    --        --xb = insert i 1 v
    --    --print $ length $ freeIxs xb
    --    --print $ freeIxs xg
    --    --print $ xxx 2 xf
    --    --print $ size xb
    --    print v
    --    print $ append2 (Mods :: Mods '[Ixed]) 1 v


--class                          Appendable2 opts cont     el out | opts cont el -> out where append2 :: Mods opts -> el -> cont -> out

    --return ()
--foo :: _ => _
--foo = append2 (Mods :: Mods '[Ixed])
--ixed append 5 c

--unsafeAppend 5 c

--unsafe append 5 c

--unsafe f opts = f (Mods :: Mods '[Unsafe])
--data
--Trans [Unsafe, Ixed]



data HSIndent  = HSIndent  deriving (Show)


runMeI = renderCode HSIndent

--tstc = runMeI ("o" <+> ("ala" <+> "ola"))

instance Repr s (VectorGraph a) where repr _ = fromString "mu"







--xxx :: _ => _
--xxx v = append v

--unsafeAppend :: Inst Appendable cont mods el out => ModFunc '[Unsafe] mods (cont -> el -> out)
--unsafeAppend = unsafe append






--type family Foo (x :: Bool) a b where
--    Foo False a b = a
--    Foo True  a b = b

--type family Bar a

--type instance Bar Int = Int

--xt = undefined :: Foo False Int (Bar String)

--xv = fromList [1,2,3] :: (Resizable Exponential (Vector Int))
xv = alloc 5 :: Reusable (Resizable Exponential (Vector Int))
--xv = fromList [1,2,3] :: Vector Int
--cont        -> el -> out


--xv2 = append (4 :: Int) xv
--xv2 = index (2 :: Int) xv
--xv2 = add 1 xv
--xv2 = ixed append 2 xv
--xv2 = maxIndex xv

xv2 = add 1 xv
xv3 = add 1 xv2
xv4 = add 1 xv3
xv5 = add 1 xv4
xv6 = add 1 xv5
xv7 = add 1 xv6

main = do
    print $ freeIxes xv
    print $ freeIxes xv2
    print $ freeIxes xv3
    print $ freeIxes xv4
    print $ freeIxes xv5
    print $ freeIxes xv6
    print $ freeIxes xv7
    --print $ freeIxes xv2
    --print $ xt
    print "end"

