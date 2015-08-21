{-# LANGUAGE UndecidableInstances #-}

module Typechecker.Label where

import           Utils.PreludePlus

import           Luna.Syntax.Graph.Builder
import           Luna.Syntax.Graph
import           Luna.Syntax.Decl
import           Luna.Syntax.Term
import           Luna.Syntax.Lit
import           Luna.Syntax.AST

import           Control.Monad.State

-- === Label ===

data Label l a = Label { _lab :: l
                       , _el  :: a
                       } deriving (Show)

class LabBuilder m l where
    mkLabel :: m l

makeLenses ''Label

-- instances

instance HasAST a ast => HasAST (Label l a) ast where
    ast = el . ast

instance {-# OVERLAPPABLE #-} (Monad m, Default a) => LabBuilder m a where
    mkLabel = return def

instance {-# OVERLAPPABLE #-} (Applicative m, LabBuilder m l, ASTGen ast m el) => ASTGen ast m (Label l el) where
    genAST a = Label <$> mkLabel <*> genAST a


