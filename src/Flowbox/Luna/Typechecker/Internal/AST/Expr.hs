module Flowbox.Luna.Typechecker.Internal.AST.Expr (Expr(..)) where

import Flowbox.Luna.Typechecker.Internal.AST.Common (ID)
import Flowbox.Luna.Typechecker.Internal.AST.Lit    (Lit)
import Flowbox.Luna.Typechecker.Internal.AST.Pat    (Pat)
import Flowbox.Luna.Typechecker.Internal.AST.Type   (Type)

-- # data Expr  = NOP          { _id :: ID                                                                                            }
-- #           -- comment | Accessor     { _id :: ID, _name      :: String   , _dst       :: Expr                                               }
-- #            | App          { _id :: ID, _src       :: Expr     , _args      :: [Expr]                                             }
-- #           -- comment | AppCons_     { _id :: ID, _args      :: [Expr]                                                                      }
-- #           -- comment | Arg          { _id :: ID, _pat       :: Pat      , _value     :: Maybe Expr                                         }
-- #            | Assignment   { _id :: ID, _pat       :: Pat      , _dst       :: Expr                                               }
-- #           -- comment | Case         { _id :: ID, _expr      :: Expr     , _match     :: [Expr]                                             }
-- #           -- comment | Con          { _id :: ID, _name      :: String                                                                      }
-- #           -- comment | Cond         { _id :: ID, _cond      :: Expr     , _success   :: [Expr] , _failure   :: Maybe [Expr]                }
-- #           ---- FIXME [wd]: name clash. ConD = Constructor Declaration. Cond = Condition
-- #           -- comment | ConD         { _id :: ID, _name      :: String   , _fields    :: [Expr]                                             }
-- #           -- comment | Data         { _id :: ID, _cls       :: Type     , _cons      :: [Expr] , _classes   :: [Expr] , _methods :: [Expr] }
-- #           -- comment | Field        { _id :: ID, _name      :: String   , _cls       :: Type   , _value     :: Maybe Expr                  }
-- #           -- comment | Function     { _id :: ID, _path      :: [String] , _name      :: String , _inputs    :: [Expr] , _output  :: Type   , _body    :: [Expr] }
-- #           -- comment | Grouped      { _id :: ID, _expr      :: Expr                                                                        }
-- #           -- comment | Import       { _id :: ID, _path      :: [String] , _target    :: Expr   , _rename    :: Maybe String                }
-- #           -- comment | ImportNative { _id :: ID, _segments  :: [Expr]                                                                      }
-- #           -- comment | Infix        { _id :: ID, _name      :: String   , _src       :: Expr   , _dst       :: Expr                        }
-- #            | Lambda       { _id :: ID, _inputs    :: [Expr]   , _output    :: Type   , _body      :: [Expr]                      }
-- #           -- comment | List         { _id :: ID, _items     :: [Expr]                                                                      }
-- #            | Lit          { _id :: ID, _lvalue    :: Lit                                                                         }
-- #           -- comment | Match        { _id :: ID, _pat       :: Pat      , _body      :: [Expr]                                             }
-- #           -- comment | Native       { _id :: ID, _segments  :: [Expr]                                                                      }
-- #           -- comment | NativeCode   { _id :: ID, _code      :: String                                                                      }
-- #           -- comment | NativeVar    { _id :: ID, _name      :: String                                                                      }
-- #           -- comment | RangeFrom    { _id :: ID, _start     :: Expr                                                                        }
-- #           -- comment | RangeFromTo  { _id :: ID, _start     :: Expr     , _end       :: Expr                                               }
-- #           -- comment | RecordUpdate { _id :: ID, _src       :: Expr     , _selectors :: [String], _expr :: Expr                            }
-- #           -- comment | Ref          { _id :: ID, _dst       :: Expr                                                                        }
-- #           -- comment | RefType      { _id :: ID, _typeName  :: String   , _name      :: String                                             }
-- #           -- comment | Tuple        { _id :: ID, _items     :: [Expr]                                                                      }
-- #           -- comment | TypeAlias    { _id :: ID, _srcType   :: Type     , _dstType   :: Type                                               }
-- #           -- comment | Typed        { _id :: ID, _cls       :: Type     , _expr      :: Expr                                               }
-- #           -- comment | TypeDef      { _id :: ID, _srcType   :: Type     , _dstType   :: Type                                               }
-- #           -- comment | Var          { _id :: ID, _name      :: String                                                                      }
-- #           -- comment | Wildcard     { _id :: ID                                                                                            }
-- #            deriving (Show, Eq)
-- # 