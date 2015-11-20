
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

-- {-# LANGUAGE PolyKinds #-}

module Main where

import Prologue hiding (simple, empty, Indexable, Simple, cons, lookup, index, children, Cons, Ixed, Repr, repr, minBound, maxBound)
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
import System.Process
import qualified Data.Text.AutoBuilder as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Constraint
import Control.Error.Util (hush)
import Data.Convert.Errors (TypeMismatch (TypeMismatch))
import Data.Constraint.Void
import Data.Variants hiding (cons)
import qualified Data.Variants as V
import Flowbox.System.Types hiding ((.:), insert, Index)
import           Control.Monad.State.Generate (newState)
import Text.Read (readMaybe)
import           Luna.Syntax.Repr.Graph
import qualified Luna.Syntax.Repr.Graph as GraphBuilder
import           Luna.Syntax.Builder
import qualified Luna.Syntax.Builder    as Builder
import qualified Luna.Syntax.Builder.Class as Builder
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
--import Luna.Diagnostic.AST (toGraphViz, display)
import Luna.Diagnostic.AST as Diag (toGraphViz, display, render, open)
import Luna.Syntax.AST.Typed
import Luna.Syntax.Layer.Labeled
import qualified Type.BaseType as BT
--import Data.Container
--import Data.Container.Hetero
import Data.Container.Resizable
import Data.Container.Reusable
--import Data.Container.Interface
--import           Data.Container.Poly {- x -} hiding (append)
import Data.Container
import Data.Container.Poly -- (Ixed)
--import Data.Text.CodeBuilder.Builder
import Data.Text.CodeBuilder.Builder as CB hiding (render, app)

import Data.Vector.Dynamic as VD

import Data.Container.Parametrized
import Data.Container.Auto
import Data.Container.Weak
import qualified Data.Container.Opts as Mods
import qualified Data.Container.Instances.Vector.Lazy as Lazy



import Data.STRef
import Control.Monad.ST
import Data.Reprx
import Data.Layer
import qualified System.Mem.Weak      as Mem
import Data.IORef

import Data.Container.Immersed
import Data.Container.Hetero (Ptr(Ptr), ptrIdx)

import Data.Container.Hetero
import Data.Layer.Coat
import qualified Luna.Syntax.Builder.Node as NodeBuilder
import           Luna.Syntax.Builder.Node (MonadNodeBuilder)
import qualified Data.IntSet as IntSet
import           Data.IntSet (IntSet)
import           Data.Construction
import qualified Language.Haskell.Session as HS
import qualified Luna.Interpreter.Session as Session


type Network = Graph (Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge)))))) DoubleArc

typed a t = StarBuilder.with (const $ Just t) a


tstx2 :: (Ref Node, Network)
tstx2 = runIdentity
      $ flip StarBuilder.evalT Nothing
      $ flip Builder.runT def
      $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
      $ do
            i2 <- _int 2
            i3 <- _int 3
            namePlus <- _string "+"
            accPlus  <- accessor namePlus i2
            nameInt  <- _string "Int"
            consInt  <- cons nameInt
            int2int  <- arrow consInt consInt
            appPlus  <- app accPlus [arg i2, arg i3] `typed` int2int
            return appPlus


runGraph gr a = runIdentityT
         $ flip StarBuilder.evalT Nothing
         $ flip Builder.runT gr
         $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int)) a


runNode (ref :: Ref Node) = do
    node <- readRef ref
    case' (uncoat (node :: Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge))))))) $ do
        match $ \(App a p) -> do
            putStrLn "App"
            acc <- follow a
            runNode acc
            -- putStrLn $ ppShow acc
            -- putStrLn $ ppShow $ inputs p
        match $ \(Accessor n s) -> do
            putStrLn "Accessor"
            -- putStrLn $ ppShow (uncoat node)
            src <- follow s
            runNode src
            -- putStrLn $ ppShow src
        match $ \(Val v) -> do
            putStrLn "Val"
            -- val <- follow v
            -- putStrLn $ ppShow val
            -- putStrLn $ ppShow v
            case' v $ do
                match $ \(Int i) -> do
                    -- return (Session.toAny i :: Any)
                    return ()
                match $ \(String s) -> do
                    -- return (Session.toAny s :: Any)
                    return ()

        -- match $ \(Cons a b) -> do
        --     putStrLn "Cons"
        --     putStrLn $ ppShow a
        --     putStrLn $ ppShow b
        -- match $ \(String s) -> do
        --     putStrLn "String"
        --
        -- match $ \(Var t) -> do
        --     putStrLn "Var"
        --     putStrLn $ ppShow t

        match $ \ANY -> do
            putStrLn "OTHER"
            putStrLn $ ppShow (uncoat node)

pass4 :: Ref Node -> Network -> IO ((), Network)
pass4 i gr = Session.run $ runGraph gr $  do

    -- putStrLn $ ppShow node
    -- node <- readRef i
    -- case' (uncoat (node :: _)) $ do
    --     match $ \(App a p) -> do
    --         acc <- follow a
    --         -- runNode acc
    --         putStrLn $ ppShow acc
    --     match $ \(Accessor n s) -> do
    --         src <- follow s
    --         -- runNode src
    --         putStrLn $ ppShow src
    --         -- putStrLn . ppShow =<< mapM readRef p
    --         -- undefined
        -- return ()
    -- uncoated <-
    runNode i
    print "foo"
    return ()
    -- where
    --     runNode node = do


main :: IO ()
main = do
    let (i, g) = tstx2
    -- let (lmap, gs) = addStdLiterals g
    -- let (unis, g2) = pass2 lmap gs
    -- let (_   , g3) = pass3 lmap unis g2

    -- renderAndOpen [ ("g" , g)
    --             --   , ("gs", gs)
    --             --   , ("g2", g2)
    --             --   , ("g3", g3)
    --               ]

    pprint g
    pass4 i g
    print "end"
