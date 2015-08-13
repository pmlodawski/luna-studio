{-# LANGUAGE UndecidableInstances #-}

module Tmp.TypecheckerTest where

import           Utils.PreludePlus

import qualified Luna.Inference as Luna
import           Luna.Inference hiding (get, put)
import           Control.Monad.State
import           Data.Repr





data Meta = Meta Int String deriving (Show)
instance Default Meta where def = Meta 0 ""

instance (MonadState Meta m) => LabBuilder m Meta where
    mkLabel = get



type FunctionGraphMeta = Function (HomoNet (Label Meta) Term)


-- ▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄


af :: FunctionGraphMeta
af = runIdentity $ flip evalStateT def $ flip execFunctionBuilderT def $ do
    a <- withMeta (Meta 7 "g") $ var "a"
    put $ Meta 1 "a"
    b <- var "b"
    put $ Meta 2 "b"
    x <- var "x" @. "foo"
    put $ Meta 3 "c"
    y <- x @$ [arg a]
    return ()



withMeta meta f = do
    old <- get
    put meta
    out <- f
    put old
    return out


main :: IO ()
main = do
    putStrLn "Typeckecker test:"
    putStrLn $ repr af
    return ()
