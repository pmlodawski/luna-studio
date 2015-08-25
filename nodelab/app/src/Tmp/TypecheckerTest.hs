{-# LANGUAGE UndecidableInstances #-}

module Tmp.TypecheckerTest where

import           Utils.PreludePlus

import           Data.Repr
import           Control.Monad.State

import           Luna.Syntax.Builder.Graph hiding (get, put)
import           Luna.Syntax.Builder

import Luna.Syntax.Layer.Typed
import Luna.Syntax.Layer.Labeled
import Luna.Syntax.AST.Term
import Luna.Syntax.AST.Decl

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


nytst2f :: RefFunctionGraphMeta
nytst2f = flip runFunctionBuilderState def $ do
    v1 <- var "foo"
    v2 <- var "bar"
    s  <- star
    a  <- v1 @. "x"
    x  <- v1 @$ [arg v2]
    y  <- x @. "y"
    return v1


varA :: StateGraphMeta -> RefFunctionGraphMeta
varA bldrState = flip runFunctionBuilderState bldrState $
    withMeta (Meta 1 "a") $ var "a"

varB :: StateGraphMeta -> RefFunctionGraphMeta
varB bldrState = flip runFunctionBuilderState bldrState $
    withMeta (Meta 2 "b") $ var "b"

accA :: GraphRefMeta -> StateGraphMeta -> RefFunctionGraphMeta
accA rv1 bldrState = flip runFunctionBuilderState bldrState $
    withMeta (Meta 3 "c") $ rv1 @. "foo"

funA :: GraphRefMeta -> GraphRefMeta -> GraphRefMeta -> StateGraphMeta -> RefFunctionGraphMeta
funA rf rv1 rv2 bldrState = flip runFunctionBuilderState bldrState $
    withMeta (Meta 4 "d") $ rf @$ [arg rv1, arg rv2]

-- funB :: GraphRefMeta -> _ -> StateGraphMeta -> RefFunctionGraphMeta
-- funB rf args bldrState = flip runFunctionBuilderState bldrState $
--     withMeta (Meta 4 "d") $ rf @$ args
-- -- example: (rv4, e) = funB rf1 [arg rv1, arg rv2] $ rebuild d


-- TODO: map id -> ref (GraphRefMeta)

rebuild :: Function g -> BldrState g
rebuild f = BldrState [] $ f ^. body

main :: IO ()
main = do
    let (rv1, a) = varA def
        (rv2, b) = varB (rebuild a)
        (rf1, c) = accA rv1 $ rebuild b
        (rv3, d) = funA rf1 rv1 rv2 $ rebuild c
    putStrLn "Typeckecker test:"
    putStrLn $ repr d
    -- putStrLn $ show rv1
    -- putStrLn $ show rv2
    -- putStrLn $ show rf1
    -- putStrLn $ show rv3

    return ()
