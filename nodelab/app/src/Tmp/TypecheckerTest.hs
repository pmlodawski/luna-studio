{-# LANGUAGE UndecidableInstances #-}

module Tmp.TypecheckerTest where

import           Utils.PreludePlus

import           Data.Repr
import           Control.Monad.State

import           Luna.Syntax.Graph.Builder.State hiding (get, put)
import           Luna.Syntax.Graph.Builder
import           Luna.Syntax.Graph
import           Luna.Syntax.Term

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

varA :: StateGraphMeta -> RefFunctionGraphMeta
varA bldrState = flip runFunctionBuilderState bldrState $ do
    a <- withMeta (Meta 1 "a") $ var "a"
    return a

varB :: StateGraphMeta -> RefFunctionGraphMeta
varB bldrState = flip runFunctionBuilderState bldrState $ do
    b <- withMeta (Meta 2 "b") $ var "b"
    return b

accA :: GraphRefMeta -> StateGraphMeta -> RefFunctionGraphMeta
accA rv1 bldrState = flip runFunctionBuilderState bldrState $ do
    f <- withMeta (Meta 3 "c") $ rv1 @. "foo"
    return f

funA :: GraphRefMeta -> GraphRefMeta -> GraphRefMeta -> StateGraphMeta -> RefFunctionGraphMeta
funA rf rv1 rv2 bldrState = flip runFunctionBuilderState bldrState $ do
    x <- withMeta (Meta 4 "d") $ rf @$ [arg rv1, arg rv2]
    return x

funB :: GraphRefMeta -> [ArgRef (GraphBuilderT GraphMeta (StateT Meta Identity)) (Label Meta) Term] -> StateGraphMeta -> RefFunctionGraphMeta
funB rf args bldrState = flip runFunctionBuilderState bldrState $ do
    x <- withMeta (Meta 4 "d") $ rf @$ args
    return x


main :: IO ()
main = do
    let (rv1, a) = varA def
        (rv2, b) = varB $ rebuild a
        (rf1, c) = accA rv1 $ rebuild b
        (rv3, d) = funA rf1 rv1 rv2 $ rebuild c
        (rv4, e) = funB rf1 [arg rv1, arg rv2] $ rebuild d
    putStrLn "Typeckecker test:"
    putStrLn $ repr e
    -- putStrLn $ show rv1
    -- putStrLn $ show rv2
    -- putStrLn $ show rf1
    -- putStrLn $ show rv3

    return ()
