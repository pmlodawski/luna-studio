{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeFamilies           #-}

module Luna.Syntax.AST where

import Flowbox.Prelude
import Luna.Syntax.AST.Term


-- === ASTOf ===

type family ASTOf a

class HasAST  a where ast :: Lens' a (ASTOf a)

-- Utils

type FromAST a = Convertible (ASTOf a) a

fromAST :: FromAST a => ASTOf a -> a
fromAST = convert 

-- Instances

type instance ASTOf (Val   t) = Val   t
type instance ASTOf (Thunk t) = Thunk t
type instance ASTOf (Term  t) = Term  t
type instance ASTOf (Draft t) = Draft t



