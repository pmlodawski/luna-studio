module Flowbox.Luna.Typechecker.Internal.AST.Type (Type(..), Tyvar(..), Tycon(..), fn, tChar, tFloat, tInteger, tString, tDouble) where

import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind         as Knd




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
          deriving (Eq, Show)

-- TODO [kgdk] 21 sie 2014: cyclic-reference
--instance Show Type where
--    show (TVar (Tyvar name kind)) = "(Var " ++ name ++ " :: " ++ show kind ++ ")"
--    show (TCon (Tycon name kind)) = "(Con " ++ name ++ " :: " ++ show kind ++ ")"
--    show (TGen i) = "(Gen " ++ show i ++ ")"
--    show (TAp x y) = "(" ++ show x ++ " -> " ++ show y ++ " :: " ++ show (on Knd.Kfun HKd.kind x y)++ ")"

-- TODO [kgdk] 14 sie 2014: lepsze instancje show, by Tyvar: "var a :: *" a Tycon: "Int :: *"

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
