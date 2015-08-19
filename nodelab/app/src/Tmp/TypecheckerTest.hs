{-# LANGUAGE UndecidableInstances #-}

module Tmp.TypecheckerTest where

import           Utils.PreludePlus

import           Data.Repr
import           Control.Monad.State

import           Luna.Syntax.Graph.Builder
import           Typechecker.Typechecker


test1 :: FunctionGraphMeta
test1 = runIdentity $ flip evalStateT def $ flip execFunctionBuilderT def $ do
    a <- withMeta (Meta 7 "g") $ var "a"
    put $ Meta 1 "a"
    b <- var "b"
    put $ Meta 2 "b"
    x <- var "x" @. "foo"
    put $ Meta 3 "c"
    y <- x @$ [arg a]
    return ()

test2 :: FunctionGraphMeta
test2 = runIdentity $ flip evalStateT def $ flip execFunctionBuilderT def $ do
    a <- withMeta (Meta 1 "a") $ var "a"
    b <- withMeta (Meta 2 "b") $ var "b"
    x <- withMeta (Meta 3 "c") $ var "x" @. "foo"
    y <- withMeta (Meta 4 "d") $ x @$ [arg a]
    return ()

n1 :: StateGraphMeta -> FunctionGraphMeta
n1 bldrState = flip execFunctionBuilderState bldrState $ do
    a <- withMeta (Meta 1 "a") $ var "a"
    return ()

n2 :: StateGraphMeta -> FunctionGraphMeta
n2 bldrState = flip execFunctionBuilderState bldrState $ do
    a <- withMeta (Meta 2 "b") $ var "b"
    return ()




main :: IO ()
main = do
    let a = n1 def
        b = n2 $ rebuild a
    putStrLn "Typeckecker test:"
    putStrLn $ repr b
    -- let c =

    return ()
