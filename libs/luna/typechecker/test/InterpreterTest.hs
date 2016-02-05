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

import           Prologue                     hiding (Cons, Indexable, Ixed, Repr, Simple, children, cons, empty, index, lookup, maxBound,
                                               minBound, repr, s, simple)

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Data.Layer.Coat
import qualified Data.Map                     as Map
import           Data.Variants                hiding (cons)
import           Data.Vector.Mutable          ()
import           Debug.Trace
import           Luna.Diagnostic.AST          as Diag (open, render, toGraphViz)
import           Luna.Interpreter.Label       (Label)
import qualified Luna.Interpreter.NodeRunner  as NodeRunner
import qualified Luna.Interpreter.Session     as Session
import           Luna.Syntax.AST.Term
import           Luna.Syntax.Builder
import qualified Luna.Syntax.Builder          as Builder
import qualified Luna.Syntax.Builder.Node     as NodeBuilder
import qualified Luna.Syntax.Builder.Star     as StarBuilder
import qualified Luna.Syntax.Builder.Symbol   as SymbolBuilder
import           Luna.Syntax.Layer.Labeled    (label)
import           Luna.Syntax.Repr.Graph
import           Luna.Syntax.Symbol.Map       (SymbolMap)
import qualified Luna.Syntax.Symbol.Map       as Symbol
import           Luna.Syntax.Symbol.Network   (Network)

-- ====================================

typed a t = StarBuilder.with (const $ Just t) a

renderAndOpen lst = do
    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)
-- ====================================


sampleGraph :: ((Ref Node, SymbolMap Label (Ref Edge)), Network Label)
sampleGraph = runIdentity
      $ flip StarBuilder.evalT Nothing
      $ flip Builder.runT def
      $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
      $ do
            nameInt  <- _string "Int"
            consInt  <- cons nameInt
            i2 <- _int 2 `typed` consInt
            i3 <- _int 3 `typed` consInt
            namePlus <- _string "+"
            accPlus  <- accessor namePlus i2
            arr      <- arrow [consInt, consInt] Map.empty consInt
            appPlus  <- app accPlus [arg i2, arg i3] `typed` arr

            nameFloat <- _string "Float"
            consFloat <- cons nameFloat
            nameInt   <- _string "Int"
            consInt   <- cons nameInt
            arr1      <- readRef =<< arrow [consInt  , consInt  ] Map.empty consInt
            arr2      <- readRef =<< arrow [consInt  , consFloat] Map.empty consInt
            arr3      <- readRef =<< arrow [consInt  , consInt  ] Map.empty consFloat
            arr4      <- readRef =<< arrow [consFloat, consInt  ] Map.empty consInt
            arr5      <- readRef =<< arrow [consFloat, consFloat] Map.empty consInt

            let mkItem arr = Symbol.PartiallySpecializedNetwork def $ Map.fromList $ map (\a -> (Symbol.fromArrow' $ matchArrow $ (uncoat a), def)) arr
            let sm = Map.fromList $ [("Int.+", mkItem ([arr1, arr2, arr3, arr4, arr5] ))]

            return (appPlus, sm)


mkSymbolMap :: Arrow t -> SymbolMap Label t
mkSymbolMap arr = Map.fromList [("Int.+", Symbol.PartiallySpecializedNetwork def $ Map.singleton (Symbol.fromArrow' arr) def )]



runGraph gr sm = runIdentityT
            . flip SymbolBuilder.evalT sm
            . flip StarBuilder.evalT Nothing
            . flip Builder.runT gr
            . flip NodeBuilder.evalT (Ref $ Node (0 :: Int))


evaluateTest :: Ref Node -> SymbolMap Label (Ref Edge) -> Network Label -> IO ((), Network Label)
evaluateTest i sm gr = Session.run $ runGraph gr sm $  do
    Right r <-  runExceptT $   NodeRunner.runNode i
    putStrLn "RESULT IS:"
    print (Session.unsafeCast r :: Int)


main :: IO ()
main = do
    let ((i, sm), g) = sampleGraph
    -- let (lmap, gs) = addStdLiterals g
    -- let (unis, g2) = pass2 lmap gs
    -- let (_   , g3) = pass3 lmap unis g2

    -- renderAndOpen [ ("g" , g)
    --             --   , ("gs", gs)
    --             --   , ("g2", g2)
    --             --   , ("g3", g3)
    --               ]
    -- renderAndOpen [ ("g" , g)]
    pprint g
    _ <- evaluateTest i sm g
    putStrLn "end"

matchArrow arr = case' arr $ match $ \a@(Arrow {}) -> a
