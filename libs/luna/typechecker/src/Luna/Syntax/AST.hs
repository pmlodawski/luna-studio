module Luna.Syntax.AST where

import Prologue hiding (cons)

--import Luna.Syntax.AST.Term 

--import           Data.Variant.Cons (cons)
--import qualified Data.Variant as Variant

--str :: Variant.Cons Str t => String -> t
--str  = cons ∘ Str
--{-# INLINABLE str #-}

--star :: Variant.Cons Star t => t
--star = cons Star
--{-# INLINABLE star #-}




---- === ASTOf ===

--type family ASTOf a

--class HasAST  a where ast :: Lens' a (ASTOf a)

---- Utils

--type FromAST a = Convertible (ASTOf a) a

--fromAST :: FromAST a => ASTOf a -> a
--fromAST = convert 

---- Instances

--type instance ASTOf (Val   t) = Val   t
--type instance ASTOf (Thunk t) = Thunk t
--type instance ASTOf (Term  t) = Term  t
--type instance ASTOf (Draft t) = Draft t



