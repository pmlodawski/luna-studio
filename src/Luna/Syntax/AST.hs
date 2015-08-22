{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeFamilies           #-}

module Luna.Syntax.AST where

import Flowbox.Prelude
import Luna.Syntax.Term


-- === HasAST ===

class HasAST a ast | a -> ast where
  ast :: Lens' a ast

instance HasAST (Val   h) (Val   h) where ast = id
instance HasAST (Thunk h) (Thunk h) where ast = id
instance HasAST (Term  h) (Term  h) where ast = id

instance HasAST (Val'   h) (Val'   h) where ast = id
instance HasAST (Thunk' h) (Thunk' h) where ast = id
instance HasAST (Term'  h) (Term'  h) where ast = id


--instance HasAST (TT a h) (TT a h) where ast = id

class (HasAST el ast, HasAST ast ast) => ASTGen ast m el where
    genAST :: ast -> m el


instance Monad m => ASTGen (Val   h) m (Val   h) where genAST = return
instance Monad m => ASTGen (Thunk h) m (Thunk h) where genAST = return
instance Monad m => ASTGen (Term  h) m (Term  h) where genAST = return

instance Monad m => ASTGen (Val'   h) m (Val'   h) where genAST = return
instance Monad m => ASTGen (Thunk' h) m (Thunk' h) where genAST = return
instance Monad m => ASTGen (Term'  h) m (Term'  h) where genAST = return



type family ASTOf (a :: * -> *) :: * -> *

type instance ASTOf Val'   = Val'
type instance ASTOf Thunk' = Thunk'
type instance ASTOf Term'  = Term'

class Monad m => LayerGen t m l where
    genLayers :: ASTOf l t -> m (l t)

--instance {-# OVERLAPPABLE #-} (Monad m, ASTOf a  => LayerGen t m a where genLayers = return

instance Monad m => LayerGen t m Term' where genLayers = return


class HasAST' l ast | l -> ast where
    ast' :: Lens' (l t) (ast t)

instance HasAST' Val'   Val'   where ast' = id
instance HasAST' Thunk' Thunk' where ast' = id
instance HasAST' Term'  Term'  where ast' = id




class HasAST2 a ast | a -> ast where
  ast2 :: Lens' (a t) (ast t)

class HasAST2 el a => ASTGen2 a m el t where
    genAST2 :: a t -> t -> m (el t)