{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeFamilies           #-}

module Luna.Syntax.AST where

import Flowbox.Prelude
import Luna.Syntax.AST.Term


-- === ASTOf ===

type family ASTOf (a :: * -> *) :: * -> *

type instance ASTOf Val   = Val
type instance ASTOf Thunk = Thunk
type instance ASTOf Term  = Term

-- === HasAST ===

class HasAST l ast | l -> ast where
    ast :: Lens' (l t) (ast t)

instance HasAST Val   Val   where ast = id
instance HasAST Thunk Thunk where ast = id
instance HasAST Term  Term  where ast = id


