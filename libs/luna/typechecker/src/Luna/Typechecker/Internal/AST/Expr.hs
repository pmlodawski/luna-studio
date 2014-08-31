module Luna.Typechecker.Internal.AST.Expr (Expr(..), tiExpr) where

import qualified Luna.Typechecker.Internal.AST.Kind         as Knd
import qualified Luna.Typechecker.Internal.AST.Lit          as Lit
import qualified Luna.Typechecker.Internal.AST.Pat          as Pat
import qualified Luna.Typechecker.Internal.AST.Type         as Ty

import qualified Luna.Typechecker.Internal.Assumptions      as Ass
import qualified Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Luna.Typechecker.Internal.Typeclasses      as Tcl
import qualified Luna.Typechecker.Internal.TypeInference    as Inf

import           Luna.Typechecker.Internal.AST.Common       (ID)

import           Text.Printf                                (printf)
import           Data.List                                  (intercalate)


data Expr = NOP          { _id :: ID                                      }
          | App          { _id :: ID, _src    :: Expr,  _args :: [Expr]   }
          | Lit          { _id :: ID, _lvalue :: Lit.Lit                  }
          | Var          { _id :: ID, _name   :: String                   }
          | Assignment   { _id :: ID, _pat    :: Pat.Pat ,  _dst  :: Expr }
          deriving (Eq)

instance Show Expr where
  show (NOP _) = "nop"
  show (App _ x xs) = printf "(%s)" $ intercalate " " $ map show (x:xs)
  show (Lit _ x) = show x
  show (Var _ x) = show x
  show (Assignment _ p x) = printf "let %s = %s" (show p) (show x)
  showList xs s = printf "%s%s" s $ unlines $ map show xs

tiExpr :: Inf.Infer Expr Ty.Type
tiExpr _  _  (NOP _) = do t <- TIM.newTVar Knd.Star
                          return ([], t)
tiExpr ce as (App _ e fs) = do (ps, te) <- tiExpr ce as e
                               qstfs    <- mapM (tiExpr ce as) fs
                               let qs  = concatMap fst qstfs
                                   tfs =       map snd qstfs
                               t <- TIM.newTVar Knd.Star
                               TIM.unify t (foldr Ty.fn te tfs)
                               return (ps++qs, t)
tiExpr _  _  (Lit _ l) = Lit.tiLit l
tiExpr _  as (Var _ name) = do sc         <- Ass.find name as
                               (ps Tcl.:=> t) <- TIM.freshInst sc
                               return (ps, t)
tiExpr _  _  (Assignment _ _ _ ) = error "AST.Expr.tiExpr: not defined for Assignment yet!"


-- #            | Accessor     { _id :: ID, _name      :: String   , _dst       :: Expr                                               }
-- #            | AppCons_     { _id :: ID, _args      :: [Expr]                                                                      }
-- #            | Arg          { _id :: ID, _pat       :: Pat      , _value     :: Maybe Expr                                         }
-- #            | Case         { _id :: ID, _expr      :: Expr     , _match     :: [Expr]                                             }
-- #            | Con          { _id :: ID, _name      :: String                                                                      }
-- #            | Cond         { _id :: ID, _cond      :: Expr     , _success   :: [Expr] , _failure   :: Maybe [Expr]                }
-- #            | ConD         { _id :: ID, _name      :: String   , _fields    :: [Expr]                                             }
-- #            | Data         { _id :: ID, _cls       :: Type     , _cons      :: [Expr] , _classes   :: [Expr] , _methods :: [Expr] }
-- #            | Field        { _id :: ID, _name      :: String   , _cls       :: Type   , _value     :: Maybe Expr                  }
-- #            | Function     { _id :: ID, _path      :: [String] , _name      :: String , _inputs    :: [Expr] , _output  :: Type   , _body    :: [Expr] }
-- #            | Grouped      { _id :: ID, _expr      :: Expr                                                                        }
-- #            | Import       { _id :: ID, _path      :: [String] , _target    :: Expr   , _rename    :: Maybe String                }
-- #            | ImportNative { _id :: ID, _segments  :: [Expr]                                                                      }
-- #            | Infix        { _id :: ID, _name      :: String   , _src       :: Expr   , _dst       :: Expr                        }
-- #            | Lambda       { _id :: ID, _inputs    :: [Expr]   , _output    :: Type   , _body      :: [Expr]                      }
-- #            | List         { _id :: ID, _items     :: [Expr]                                                                      }
-- #            | Match        { _id :: ID, _pat       :: Pat      , _body      :: [Expr]                                             }
-- #            | Native       { _id :: ID, _segments  :: [Expr]                                                                      }
-- #            | NativeCode   { _id :: ID, _code      :: String                                                                      }
-- #            | NativeVar    { _id :: ID, _name      :: String                                                                      }
-- #            | RangeFrom    { _id :: ID, _start     :: Expr                                                                        }
-- #            | RangeFromTo  { _id :: ID, _start     :: Expr     , _end       :: Expr                                               }
-- #            | RecordUpdate { _id :: ID, _src       :: Expr     , _selectors :: [String], _expr :: Expr                            }
-- #            | Ref          { _id :: ID, _dst       :: Expr                                                                        }
-- #            | RefType      { _id :: ID, _typeName  :: String   , _name      :: String                                             }
-- #            | Tuple        { _id :: ID, _items     :: [Expr]                                                                      }
-- #            | TypeAlias    { _id :: ID, _srcType   :: Type     , _dstType   :: Type                                               }
-- #            | Typed        { _id :: ID, _cls       :: Type     , _expr      :: Expr                                               }
-- #            | TypeDef      { _id :: ID, _srcType   :: Type     , _dstType   :: Type                                               }
-- #            | Wildcard     { _id :: ID                                                                                            }
-- # 
