module Luna.Typechecker.Internal.AST.Type (
    Type(..), Tyvar(..), Tycon(..),
    fn, pair, list, tUnit, tChar, tInt, tInteger, tFloat, tDouble, tList, tArrow, tTuple2, tString
  ) where

import qualified Luna.Typechecker.Internal.AST.Kind         as Knd


-- | The type of an expression.
data Type = TVar Tyvar    -- ^ Type variable (named unknown).
          | TCon Tycon    -- ^ Type constant (Int, Char, etc.).
          | TAp Type Type -- ^ Type application ( 'TAp [] Int' means '[Int]').
          | TGen Int      -- ^ Used for quantified variables ('a' in 'forall. a b => a -> b -> a').
          deriving (Eq, Show)


-- | Type variable.
data Tyvar = Tyvar String Knd.Kind -- ^ Type variable consists of name and kind.
           deriving (Show, Eq)

-- | Type constant.
data Tycon = Tycon String Knd.Kind -- ^ Type constant consists of name and kind.
           deriving (Show, Eq)

tUnit, tChar, tInt, tInteger, tFloat, tDouble, tList, tArrow, tTuple2, tString :: Type
tUnit    = TCon (Tycon "()"      Knd.Star)
tChar    = TCon (Tycon "Char"    Knd.Star)
tInt     = TCon (Tycon "Int"     Knd.Star)
tInteger = TCon (Tycon "Integer" Knd.Star)
tFloat   = TCon (Tycon "Float"   Knd.Star)
tDouble  = TCon (Tycon "Double"  Knd.Star)

tList    = TCon (Tycon "[]"      (Knd.Kfun Knd.Star Knd.Star))
tArrow   = TCon (Tycon "(->)"    (Knd.Kfun Knd.Star (Knd.Kfun Knd.Star Knd.Star)))
tTuple2  = TCon (Tycon "(,)"     (Knd.Kfun Knd.Star (Knd.Kfun Knd.Star Knd.Star)))

tString  = list tChar

infixr 4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b

list :: Type -> Type
list = TAp tList

pair :: Type -> Type -> Type
pair = TAp . TAp tTuple2
