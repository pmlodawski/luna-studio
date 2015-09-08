
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

import Flowbox.Prelude hiding (simple, empty, Indexable, Simple, cons, lookup, index, Wrapped, children, Cons)
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
import Flowbox.System.Types hiding ((.:))
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


--import Data.Text.CodeBuilder.Builder
import Data.Text.CodeBuilder.Builder as CB


-- === HomoBuilder ===

newtype HomoG (t :: * -> *) m a = HomoG { fromHomoG :: IdentityT m a } deriving (MonadFix, Monad, Functor, Applicative, MonadTrans)

runHomoBuilder :: HomoG t m a -> m a
runHomoBuilder = runIdentityT . fromHomoG


instance (MuBuilder a m t, t ~ t') => MuBuilder a (HomoG t m) t' where
    buildMu = lift . buildMu

-------------------------------------------------------------

nytst2 :: (Arc (Labeled Int (Typed Draft)), HomoGraph ArcPtr (Labeled Int (Typed Draft)))
nytst2 = flip runGraph def $ do
    genTopStar
    i1   <- int 1
    i2   <- blank
    plus <- i1 @. "+"
    sum  <- plus @$ [arg i1, arg i2]
    return i1

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

(_, gr) = nytst2

gr2 :: ((), HomoGraph ArcPtr (Labeled Int (Typed Draft)))
gr2 = flip runGraph (BldrState [] gr) $ do
    --genTopStar
    --i1   <- int 1
    --g <- view graph <$> GraphBuilder.get
    withGraphM_ $ \g -> flip mapM g $ \(Labeled l (Typed t ast)) -> do
        xx <- case' ast $ do
            match $ \case
                Int    i -> string "foo1"
                String s -> string "foo2"
            match $ \ANY -> string "foo3"
        i <- string "xxx"
        return (Labeled l (Typed t ast))
    return ()

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

main = do
    --putStrLn $ repr y
    print $ repr $ nytst2
    --putStrLn $ repr $ nytst3
    --print c'
    --print $ take 1000 names

    let gv = toGraphViz (snd gr2)
    print   gv
    display gv
    --V.test
    --BT.test
    --print tstc
    return ()




data HSIndent  = HSIndent  deriving (Show)


runMeI = renderCode HSIndent

--tstc = runMeI ("o" <+> ("ala" <+> "ola"))

instance Repr s (VectorGraph a) where repr _ = fromString "mu"