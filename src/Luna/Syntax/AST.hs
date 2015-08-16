{-# LANGUAGE FunctionalDependencies #-}

module Luna.Syntax.AST where

import Flowbox.Prelude
import Luna.Syntax.Term


-- === HasAST ===

class HasAST a ast | a -> ast where
  ast :: Lens' a ast

instance HasAST (Val   h) (Val   h) where ast = id
instance HasAST (Thunk h) (Thunk h) where ast = id
instance HasAST (Term  h) (Term  h) where ast = id


--instance HasAST (TT a h) (TT a h) where ast = id

class HasAST el ast => ASTGen ast m el where
    genAST :: ast -> m el


instance Monad m => ASTGen (Val   h) m (Val   h) where genAST = return
instance Monad m => ASTGen (Thunk h) m (Thunk h) where genAST = return
instance Monad m => ASTGen (Term  h) m (Term  h) where genAST = return

--instance Monad m => ASTGen (TT a h) m (TT a h) where genAST = return