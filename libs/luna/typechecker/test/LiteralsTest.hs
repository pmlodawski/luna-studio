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
import           Luna.Syntax.AST.Arg
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
    labelAttrs n = if n ^. label . Label.checked
        then [] -- [GV.color GVC.Magenta]
        else []

-- ====================================

emptyArgList :: [Arg (Ref Node)]
emptyArgList = []

emptyNodeList :: [Ref Node]
emptyNodeList = []

sampleGraph :: ((Ref Node, SymbolMap (Network Label)), Network Label)
sampleGraph = runIdentity
      $ flip StarBuilder.evalT Nothing
      $ flip Builder.runT def
      $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
      $ do
            nameInt       <- _string "Int"
            nameString    <- _string "String"
            namePlus      <- _string "+"
            nameConc      <- _string "++"
            nameLen       <- _string "len"

            consIntTpe    <- cons nameInt
            consStringTpe <- cons nameString

            arrPlusTpe    <- arrow [consIntTpe] Map.empty consIntTpe
            arrConcTpe    <- arrow [consStringTpe] Map.empty consStringTpe
            arrLenTpe     <- arrow emptyNodeList Map.empty consIntTpe

            i1 <- _int 2 -- `typed` arrLenTpe
            i2 <- _int 3
            i3 <- _int 4
            s1 <- _stringVal "abc" -- `typed` consStringTpe
            s2 <- _stringVal "def"
            s3 <- _stringVal "ghi"

            accPlus1a  <- accessor namePlus i1
            appPlus1a  <- app accPlus1a [arg i2] `typed` arrPlusTpe

            accPlus1b  <- accessor namePlus i3
            appPlus1b  <- app accPlus1b [arg appPlus1a] `typed` arrPlusTpe

            accConc1a  <- accessor nameConc s2
            appConc1a  <- app accConc1a [arg s1] `typed` arrConcTpe

            accConc1b  <- accessor nameConc appConc1a
            appConc1b  <- app accConc1b [arg s3] `typed` arrConcTpe

            accLen    <- accessor nameLen appConc1b
            appLen    <- app accLen emptyArgList `typed` arrLenTpe

            accPlus2  <- accessor namePlus appPlus1b
            appPlus2  <- app accPlus2 [arg appLen] `typed` arrPlusTpe

            return (appPlus1b, def)
            -- return (appPlus2, def)

runGraph gr sm = runIdentityT
            . flip SymbolBuilder.evalT sm
            . flip StarBuilder.evalT Nothing
            . flip Builder.runT gr
            . flip NodeBuilder.evalT (Ref $ Node (0 :: Int))

literalsTest :: Ref Node -> SymbolMap (Network Label) -> Network Label -> IO ((), Network Label)
literalsTest i sm gr = runGraph gr sm $ do
    Literals.assignLiteralTypes i
    return ()

main :: IO ()
main = do

    let ((i, sm), g) = sampleGraph
    ((), g')<- literalsTest i sm g
    -- pprint g'
    -- renderAndOpen [ ("g" , g)]
    renderAndOpen [ ("g" , g')]
    putStrLn "end"
