{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedStrings       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Main where

import Prologue hiding (Cons, Indexable, Ixed, Repr, Simple, children, cons, empty, index, lookup, maxBound, minBound, repr, s, simple)

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import qualified Data.GraphViz.Attributes            as GV
import qualified Data.GraphViz.Attributes.Colors.X11 as GVC
import           Data.Layer.Coat
import qualified Data.Map                            as Map
import           Data.Variants                       hiding (cons)
import           Data.Vector.Mutable                 ()
import           Debug.Trace
import           Development.Placeholders

import           Luna.Diagnostic.AST          as Diag (LabelAttrs (..), open, render, toGraphViz)
import qualified Luna.Inference.Literals      as Literals
import qualified Luna.Interpreter.Interpreter as Interpreter
import           Luna.Interpreter.Label       (Label)
import qualified Luna.Interpreter.Label       as Label
import qualified Luna.Interpreter.Monad       as InterpreterMonad
import qualified Luna.Interpreter.NodeRunner  as NodeRunner
import qualified Luna.Interpreter.Session     as Session
import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Typed        (Typed)
import           Luna.Syntax.Builder
import qualified Luna.Syntax.Builder          as Builder
import qualified Luna.Syntax.Builder.Node     as NodeBuilder
import qualified Luna.Syntax.Builder.Star     as StarBuilder
import qualified Luna.Syntax.Builder.Symbol   as SymbolBuilder
import           Luna.Syntax.Layer.Labeled    (label)
import           Luna.Syntax.Layer.Labeled    (Labeled2)
import           Luna.Syntax.Repr.Graph
import           Luna.Syntax.Symbol.Map       (SymbolMap)
import qualified Luna.Syntax.Symbol.Map       as Symbol
import           Luna.Syntax.Symbol.Network   (Network)

-- ====================================

typed a t = StarBuilder.with (const $ Just t) a

renderAndOpen lst = do
    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)

instance LabelAttrs (Labeled2 Label (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge)))))) where
    labelAttrs n = if n ^. label . Label.dirty
        then [GV.color GVC.Brown]
        else []
-- ====================================


sampleGraph :: ((Ref Node, SymbolMap Label (Ref Edge)), Network Label)
sampleGraph = runIdentity
      $ flip StarBuilder.evalT Nothing
      $ flip Builder.runT def
      $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
      $ do
            nameString <- _string "String"
            consString <- cons nameString
            nameInt    <- _string "Int"
            consInt    <- cons nameInt
            s1 <- _stringVal "abc" `typed` consString
            s2 <- _stringVal "def"
            i2 <- _int 2
            i3 <- _int 3
            i4 <- _int 4 `typed` consInt
            namePlus <- _string "+"
            accPlus  <- accessor namePlus i2
            arr      <- arrow [consInt, consInt] Map.empty consInt
            appPlus  <- app accPlus [arg i2, arg i3] `typed` arr

            let sm = def

            return (s2, sm)

runGraph gr sm = runIdentityT
            . flip SymbolBuilder.evalT sm
            . flip StarBuilder.evalT Nothing
            . flip Builder.runT gr
            . flip NodeBuilder.evalT (Ref $ Node (0 :: Int))


evaluateTest :: Ref Node -> SymbolMap Label (Ref Edge) -> Network Label -> IO ((), Network Label)
evaluateTest i sm gr = Session.run $ runGraph gr sm $  do
    Just r <- NodeRunner.runNode def i
    putStrLn "RESULT IS:"
    print (Session.unsafeCast r :: Int)


literalsTest :: Ref Node -> SymbolMap Label (Ref Edge) -> Network Label -> IO ((), Network Label)
literalsTest i sm gr = runGraph gr sm $ do
    Literals.assignLiteralTypes i
    return ()

main :: IO ()
main = do
    let ((i, sm), g) = sampleGraph
    ((), g')<- literalsTest i sm g
    -- pprint g'
    renderAndOpen [ ("g" , g')]
    putStrLn "end"

matchArrow arr = case' arr $ match $ \a@(Arrow {}) -> a
