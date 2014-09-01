module Luna.Typechecker.Internal.AST.Type (
    Type(..), Tyvar(..), Tycon(..),
    fn, pair, list, tUnit, tChar, tBool, tInt, tInteger, tFloat, tDouble, tList, tArrow, tTuple2, tString, tCons
  ) where

import           Luna.Typechecker.Internal.AST.Kind         (Kind(..))
import           Luna.Typechecker.Internal.AST.TID          (TID)

import           Text.Printf                                (printf)

-- | The type of an expression.
data Type = TVar Tyvar    -- ^ Type variable (named unknown).
          | TCon Tycon    -- ^ Type constant (Int, Char, etc.).
          | TAp Type Type -- ^ Type application ( 'TAp [] Int' means '[Int]').
          | TGen Int      -- ^ Used for quantified variables ('a' in 'forall. a b => a -> b -> a').
          deriving (Eq)

instance Show Type where
  show (TVar (Tyvar n k)) = printf "(%s::%s)" n (show k)
  show (TCon (Tycon n k)) = printf "(%s::%s)" (show n) (show k)
  show (TAp (TAp x t1) t2)
    | x == tArrow         = printf "%s -> %s" (show t1) (show t2)
  show (TAp t1 t2)        = printf "(%s %s)" (show t1) (show t2)
  show (TGen i)           = printf "gen{%d}" i
  showList cs s = printf "%s%s" s (unwords $ map show cs)


-- | Type variable.
data Tyvar = Tyvar TID Kind -- ^ Type variable consists of name and kind.
           deriving (Eq)

instance Show Tyvar where
  show x = show (TVar x)

-- | Type constant.
data Tycon = Tycon TID Kind -- ^ Type constant consists of name and kind.
           deriving (Eq)

instance Show Tycon where
  show x = show (TCon x)

tUnit, tChar, tBool, tInt, tInteger, tFloat, tDouble, tList, tArrow, tTuple2, tString :: Type
tUnit    = TCon (Tycon "()"      Star)
tBool    = TCon (Tycon "Bool"    Star)
tChar    = TCon (Tycon "Char"    Star)
tInt     = TCon (Tycon "Int"     Star)
tInteger = TCon (Tycon "Integer" Star)
tFloat   = TCon (Tycon "Float"   Star)
tDouble  = TCon (Tycon "Double"  Star)

tList    = TCon (Tycon "[]"      (Kfun Star Star))
tArrow   = TCon (Tycon "(->)"    (Kfun Star (Kfun Star Star)))
tTuple2  = TCon (Tycon "(,)"     (Kfun Star (Kfun Star Star)))

tString  = list tChar

tCons :: Type -> Type
tCons  a = a `fn` list a `fn` list a

infixr 4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b
-- a `fn` b | kind a == Star && kind b == Star = TAp (TAp tArrow a) b
-- a `fn` b = error "nope"

list :: Type -> Type
list = TAp tList

pair :: Type -> Type -> Type
pair = TAp . TAp tTuple2
