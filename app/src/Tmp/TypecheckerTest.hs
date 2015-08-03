{-# LANGUAGE CPP #-}


-- TODO: Remove entire file
module Tmp.TypecheckerTest where

import           Utils.PreludePlus



-- #define TYPECHECKER_TEST


#ifdef TYPECHECKER_TEST
-- Necessary to pull changes on __flowbox project__, switch to typechecker branch and from nodelab run:
-- cabal install ../flowbox/libs/convert ../flowbox/libs/utils ../flowbox/third-party/graphviz-2999.17.0.2.2 ../flowbox/libs/luna/typechecker --ghcjs
-- where ../flowbox - __flowbox project__
import           Luna.Inference hiding (main)
import qualified Luna.Inference as TC
import qualified Data.Map       as Map

funck :: Function (GFBody NodePtr)
funck = buildFunction gra1

-- gra1 :: ASTBuilder m NodePtr Expr => m (Ref NodePtr Expr)
-- gra1 = do .... return a
gra1 :: ASTBuilder m NodePtr Expr => m ()
gra1 = do
    -- a    <- ref "foo" $ var "a"
    -- b    <- ref "bar" $ var "b"
    -- c    <- ref "baz" $ var "c"
    -- return ()
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
    let bodyF  = funck & view body
        graphF = funck & view fgraph
        Just (Ref ptrF) = Map.lookup "Main" bodyF
        regF = graphF ^. reg
        varF = unsafeGet ptrF regF

    print funck
    print regF
    print varF


-- Function {
--             _body = fromList [
--                 ("bar",Ref (NodePtr {__ptr = Ptr "Expr NodePtr" 1})),
--                 ("baz",Ref (NodePtr {__ptr = Ptr "Expr NodePtr" 2})),
--                 ("foo",Ref (NodePtr {__ptr = Ptr "Expr NodePtr" 0}))],
--             _fgraph = Graph {
--                 _reg = HContainer {
--                     _elems = fromList [
--                         Var {_name = "a"},
--                         Var {_name = "b"},
--                         Var {_name = "c"}]
--                     }
--                 }
--


#else
tctest :: IO ()
tctest = putStrLn "Testing typechecker disabled"
#endif
