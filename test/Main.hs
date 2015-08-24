
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

module Main where

import Flowbox.Prelude hiding (simple, empty, Indexable, Simple, cons, lookup, index, Wrapped, children, Cons)
import Data.Repr

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


-- === HomoBuilder ===

newtype HomoG (t :: * -> *) m a = HomoG { fromHomoG :: IdentityT m a } deriving (MonadFix, Monad, Functor, Applicative, MonadTrans)

runHomoBuilder :: HomoG t m a -> m a
runHomoBuilder = runIdentityT . fromHomoG


instance (MuBuilder a m t, t ~ t') => MuBuilder a (HomoG t m) t' where
    buildMu = lift . buildMu

-------------------------------------------------------------

nytst2 :: (Arc (Typed Term), HomoGraph ArcPtr (Typed Term))
nytst2 = flip runGraph def  $ do
    v1 <- var "foo"
    v2 <- var "bar"
    s  <- star
    a  <- v1 @. "x"
    x  <- v1 @$ [arg v2]
    y  <- x @. "y"
    return v1

nytst3 :: Mu (Typed Term)
nytst3 = flip StarBuilder.eval Nothing $ runIdentityT $ do
    v1 <- var "foo"
    return v1

main = do
    --putStrLn $ repr y
    putStrLn $ repr $ nytst2
    --putStrLn $ repr $ nytst3
    --print c'
    --print $ take 1000 names

    let gv = toGraphViz (snd nytst2)
    print gv
    display gv
    V.test
    return ()

