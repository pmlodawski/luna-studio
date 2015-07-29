{-# LANGUAGE CPP #-}


-- TODO: Remove entire file
module Tmp.TypecheckerTest where

import           Utils.PreludePlus



-- #define TYPECHECKER_TEST


#ifdef TYPECHECKER_TEST
-- Necessary to pull changes on __flowbox project__, switch to typechecker branch and from flowbox-gui run:
-- cabal install ../flowbox/libs/convert ../flowbox/libs/utils ../flowbox/third-party/graphviz-2999.17.0.2.2 ../flowbox/libs/luna/typechecker --ghcjs
-- where ../flowbox - __flowbox project__
import           Luna.Inference hiding (main)
import qualified Luna.Inference as TC
import qualified Data.Map       as Map


funck :: Function (GFBody NodePtr)
funck = buildFunction gra1

gra1 :: ASTBuilder m NodePtr Expr => m (Ref NodePtr Expr)
gra1 = do
    a    <- ref "foo" $ var "a"
    b    <- ref "bar" $ var "b"
    return a
    --mod  <- var "Main"
    --foo  <- a    @.  "foo"
    --b    <- foo  @$$ [a]
    --bar  <- mod  @.  "bar"
    --c    <- bar  @$$ [b]
    --plus <- mod  @.  "plus"
    --out  <- plus @$$ [c, a]
    --return ()

tctest :: IO ()
tctest = do
    putStrLn "Testing typechecker"
    let m = fck & view body
        g = fck & view fgraph
        Just (Ref p1) = Map.lookup "foo" m
        r = g ^. reg
        e = unsafeGet p1 r

    print fck
    print e

#else
tctest :: IO ()
tctest = putStrLn "Testing typechecker disabled"
#endif
