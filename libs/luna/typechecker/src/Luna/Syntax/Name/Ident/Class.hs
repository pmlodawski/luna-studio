module Luna.Syntax.Name.Ident.Class where

import Prelude.Luna

import Data.Char                   (isLower, isUpper)
import Luna.Syntax.Name.Class
import Luna.Syntax.Name.Ident.Type

-------------------
-- === Ident === --
-------------------

-- === Definition === --

newtype Ident t = Ident NameBase deriving (Show, Eq, Ord)
makeWrapped ''Ident

type VarIdent  = Ident Var
type TypeIdent = Ident Type


-- === Utils === --

varIdent :: String -> Ident Var
varIdent = wrap' ∘ make
{-# INLINE varIdent #-}

typeIdent :: String -> Ident Type
typeIdent = wrap' ∘ make
{-# INLINE typeIdent #-}


-- === Instances === --

type instance Name (Ident t) =  Ident t
instance   HasName (Ident t) where name = id ; {-# INLINE name #-}

instance Repr s (Ident t) where repr = repr ∘ unwrap'
