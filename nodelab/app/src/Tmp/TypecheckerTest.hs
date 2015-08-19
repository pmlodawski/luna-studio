{-# LANGUAGE UndecidableInstances #-}

module Tmp.TypecheckerTest where

import           Utils.PreludePlus

-- change imports
import qualified Luna.Inference as Luna
import           Luna.Inference hiding (get, put)
import           Control.Monad.State
import           Data.Repr



import Data.GraphViz.Types.Canonical
import Data.GraphViz.Attributes.Complete hiding (Label, Int)
import qualified Data.GraphViz.Attributes.Complete as GV
import Data.GraphViz.Printing (toDot)
import Data.GraphViz.Commands


data Meta = Meta Int String deriving (Show)
instance Default Meta where def = Meta 0 ""

instance (MonadState Meta m) => LabBuilder m Meta where
    mkLabel = get



type FunctionGraphMeta = Function (HomoNet (Label Meta) Term)


withMeta meta f = do
    old <- get
    put meta
    out <- f
    put old
    return out

-- ▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄


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


main :: IO ()
main = do
    putStrLn "Typeckecker test:"
    putStrLn $ repr test2
    return ()
