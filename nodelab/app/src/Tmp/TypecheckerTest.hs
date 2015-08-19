{-# LANGUAGE UndecidableInstances #-}

module Tmp.TypecheckerTest where

import           Utils.PreludePlus

-- change imports
-- import qualified Luna.Inference as Luna
-- import           Luna.Inference hiding (get, put)

import           Luna.Syntax.Graph.Builder
import           Luna.Syntax.Graph
import           Luna.Syntax.Term
import           Luna.Syntax.Lit
import           Luna.Syntax.AST
import           Luna.Syntax.Decl

-- import           Luna.Syntax.Name.Pool


import           Control.Monad.State
import           Data.Repr



import Data.GraphViz.Types.Canonical
import Data.GraphViz.Attributes.Complete hiding (Label, Int)
import qualified Data.GraphViz.Attributes.Complete as GV
import Data.GraphViz.Printing (toDot)
import Data.GraphViz.Commands

-- ▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄


data Meta = Meta Int String deriving (Show)
instance Default Meta where def = Meta 0 ""

-- ▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄

-- === Label ===

data Label l a = Label { _lab :: l, _el :: a } deriving (Show)

class LabBuilder m l where
    mkLabel :: m l

makeLenses ''Label

-- instances

instance HasAST a ast => HasAST (Label l a) ast where
    ast = el . ast

instance {-# OVERLAPPABLE #-} (Monad m, Default a) => LabBuilder m a where
    mkLabel = return def

instance {-# OVERLAPPABLE #-} (MonadState Meta m) => LabBuilder m Meta where
    mkLabel = get

instance {-# OVERLAPPABLE #-} (Applicative m, LabBuilder m l, ASTGen ast m el) => ASTGen ast m (Label l el) where
    genAST a = Label <$> mkLabel <*> genAST a

-- ▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄▀▄


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

n1 :: FunctionGraphMeta
n1 = runIdentity $ flip evalStateT def $ flip execFunctionBuilderT def $ do
    a <- withMeta (Meta 1 "a") $ var "a"
    return ()

n2 :: FunctionGraphMeta
n2 = runIdentity $ flip evalStateT def $ flip execFunctionBuilderT def $ do
    a <- withMeta (Meta 2 "b") $ var "b"
    return ()

-- n3 :: FunctionGraphMeta


main :: IO ()
main = do
    putStrLn "Typeckecker test:"
    putStrLn $ repr n1
    putStrLn $ repr n2
    return ()
