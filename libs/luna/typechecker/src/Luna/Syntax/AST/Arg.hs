module Luna.Syntax.AST.Arg where

import Prelude.Luna

import Luna.Syntax.Name.Ident

data Arg a = Arg { __aname :: Maybe VarIdent , __arec :: a } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


instance Repr s a => Repr s (Arg a) where repr (Arg n a) = "Arg" <+> repr n <+> repr a


--data NamedArg a = NamedArg VarIdent a deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
