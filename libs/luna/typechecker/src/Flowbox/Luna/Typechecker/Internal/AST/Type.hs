module Flowbox.Luna.Typechecker.Internal.AST.Type (
    Type(..),
    Tyvar(..), Tycon(..),
    tUnit, tChar, tInt, tInteger, tFloat, tDouble, tList, tArrow, tTuple2, tString,
    fn, list, pair
  ) where

import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind as Knd

-- #     Type(..), Tyvar(..), Tycon(..),
-- #     tUnit, tChar, tInt, tInteger, tFloat, tDouble, tList, tArrow, tTuple2, tString,
-- #     fn, list, pair
-- #     ) where
-- # 
-- # import Flowbox.AST.Common (ID)
-- # import Flowbox.AST.Kind   (Kind(..))
-- # 
-- # --data Type = App     { _id :: ID, _src      :: Type     , _args    :: [Type]   }
-- # --          | Con     { _id :: ID, _segments :: [String]                        }
-- # ----           | Data    { _id :: ID, _name     :: String   , _params  :: [String] }
-- # ----           | Lambda  { _id :: ID, _inputs   :: [Type]   , _output  :: Type     }
-- # ----           | Module  { _id :: ID, _name     :: String   , _path    :: [String] }
-- # ----           | Tuple   { _id :: ID, _items    :: [Type]                          }
-- # ----           | Unknown { _id :: ID                                               }
-- # --          | Var     { _id :: ID, _name     :: String                          }
-- # --          deriving (Show, Eq)
-- # 
-- # 
-- # -- TODO: zmienić 'TVar' na 'Var'

-- | The type of an expression.
data Type = TVar Tyvar    -- ^ Type variable (named unknown).
          | TCon Tycon    -- ^ Type constant (Int, Char, etc.).
          | TAp Type Type -- ^ Type application ( 'TAp [] Int' means '[Int]').
          | TGen Int      -- ^ Used for quantified variables ('a' in 'forall. a b => a -> b -> a').
          deriving (Eq)

instance Show Type where
    show (TVar (Tyvar name kind)) = "(Var " ++ name ++ " :: " ++ show kind ++ ")"
    show (TCon (Tycon name kind)) = "(Con " ++ name ++ " :: " ++ show kind ++ ")"
    show (TGen i) = "(Gen " ++ show i ++ ")"
    show (TAp x y) = "(" ++ show x ++ " -> " ++ show y ++ " :: " ++ show (on Kfun kind x y)++ ")"

-- TODO [kgdk] 14 sie 2014: lepsze instancje show, by Tyvar: "var a :: *" a Tycon: "Int :: *"

-- | Type variable.
data Tyvar = Tyvar String Kind -- ^ Type variable consists of name and kind.
           deriving (Show, Eq)

-- | Type constant.
data Tycon = Tycon String Kind -- ^ Type constant consists of name and kind.
           deriving (Show, Eq)

tUnit, tChar, tInt, tInteger, tFloat, tDouble, tList, tArrow, tTuple2, tString :: Type
tUnit    = TCon (Tycon "()"      Star)
tChar    = TCon (Tycon "Char"    Star)
tInt     = TCon (Tycon "Int"     Star)
tInteger = TCon (Tycon "Integer" Star)
tFloat   = TCon (Tycon "Float"   Star)
tDouble  = TCon (Tycon "Double"  Star)

tList    = TCon (Tycon "[]"      (Kfun Star Star))
tArrow   = TCon (Tycon "(->)"    (Kfun Star (Kfun Star Star)))
tTuple2  = TCon (Tycon "(,)"     (Kfun Star (Kfun Star Star)))

tString  = list tChar

infixr 4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b

list :: Type -> Type
list = TAp tList

pair :: Type -> Type -> Type
pair = TAp . TAp tTuple2





class Types t where
    apply :: Subst -> t -> t -- ^ Substitutions can be applied to types-and, in fact, to any other value with type components-in a natural way.
    tv :: t -> [Ty.Tyvar]    -- ^ Returns the set of type variables (i.e., Tyvars) appearing in its argument, listed in order of first occurrence (from left to right), with no duplicates.


instance Types Type where
  apply s (Ty.TVar u)  = fromMaybe (Ty.TVar u) (lookup u s)
  apply s (Ty.TAp l r) = Ty.TAp (apply s l) (apply s r)
  apply _ t            = t -- no substitution for TGen and TCon
  tv (Ty.TVar u)  = [u]
  tv (Ty.TAp l r) = tv l `union` tv r
  tv _            = [] -- no type variables from TGen and TCon

-- TODO [kgdk] 14 sie 2014: czy apply/tv dla TGen/TCon powinno rzucać error czy działać?


instance Types a => Types [a] where
  apply s = map (apply s)
  tv = nub . concatMap tv -- O(n^2)

-- TODO [kgdk] 14 sie 2014: poprawić implementację 'tv', by była w czasie O(n log n). Na 95% konieczne będzie
-- zachowanie kolejności elementów.

