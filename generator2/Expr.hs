{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE TemplateHaskell           #-}

{-# LANGUAGE TypeFamilies              #-}
-- {-# LANGUAGE FunctionalDependencies    #-}

module Expr where

import AST.Lit
import Data.Int
import GHC.Generics        (Generic)

type ID = Int64
type Pat = Int64
--type Lit = Int64
type Type = String


-- type Arg a = [a]

type EvilArg a = [a]

data Arg a = Named   { _iid :: ID, _naame :: String, _aarg :: a }
           | Unnamed { _iid :: ID, _aarg :: a                  }
           | NestingEvil { _evil :: EvilArg a }
           deriving (Show, Eq, Generic, Read)

data Name = NameA String Int Double | NameB (Maybe (Maybe (Maybe Expr)))
           deriving (Show, Eq, Generic, Read)

data Expr  = NOP          { _id :: ID                                                                                            }
           | Accessor     { _id :: ID, _acc       :: Accessor , _dst       :: Expr                                               }
           | TypeAlias    { _id :: ID, _srcType   :: Type     , _dstType   :: Type                                               }
           | TypeDef      { _id :: ID, _srcType   :: Type     , _dstType   :: Type                                               }
           | App          { _id :: ID, _src       :: Expr     , _args      :: [Arg Expr]                                         }
           -- | AppCons_     { _id :: ID, _args      :: [Expr]                                                                      }
           | Assignment   { _id :: ID, _pat       :: Pat      , _dst       :: Expr                                               }
           | RecordUpdate { _id :: ID, _src       :: Expr     , _selectors :: [String], _expr :: Expr                            }
           | Data         { _id :: ID, _cls       :: Type     , _cons      :: [Expr] , _classes   :: [Expr] , _methods :: [Expr] }
           | DataNative   { _id :: ID, _cls       :: Type     , _cons      :: [Expr] , _classes   :: [Expr] , _methods :: [Expr] }
           -- FIXME [wd]: name clash. ConD = Constructor Declaration. Cond = Condition
           | ConD         { _id :: ID, _name      :: String   , _fields    :: [Expr]                                             }
           | Con          { _id :: ID, _name      :: String                                                                      }
           | Cond         { _id :: ID, _cond      :: Expr     , _success   :: [Expr] , _failure   :: Maybe [Expr]                }
           | Function     { _id :: ID, _path      :: [String] , _fname     :: Name , _inputs    :: [Expr] , _output  :: Type   , _body    :: [Expr] }
           | Lambda       { _id :: ID, _inputs    :: [Expr]   , _output    :: Type   , _body      :: [Expr]                      }
           | Grouped      { _id :: ID, _expr      :: Expr                                                                        }
           | Import       { _id :: ID, _path      :: [String] , _target    :: Expr   , _rename    :: Maybe String                }
           | ImportNative { _id :: ID, _segments  :: [Expr]                                                                      }
           | Infix        { _id :: ID, _name      :: String   , _src       :: Expr   , _dst       :: Expr                        }
           | List         { _id :: ID, _items     :: [Expr]                                                                      }
           | Lit          { _id :: ID, _lvalue    :: Lit                                                                         }
           | Tuple        { _id :: ID, _items     :: [Expr]                                                                      }
           -- | TupleCons_   { _id :: ID, _items     :: [Expr]                                                                      }
           | Typed        { _id :: ID, _cls       :: Type     , _expr      :: Expr                                               }
           | Var          { _id :: ID, _name      :: String                                                                      }
           | FuncVar      { _id :: ID, _fname     :: Name                                                                        }
           | Wildcard     { _id :: ID                                                                                            }
           | RangeFromTo  { _id :: ID, _start     :: Expr     , _end       :: Expr                                               }
           | RangeFrom    { _id :: ID, _start     :: Expr                                                                        }
           | Field        { _id :: ID, _name      :: String   , _cls       :: Type   , _value     :: Maybe Expr                  }
           | Arg          { _id :: ID, _pat       :: Pat      , _value     :: Maybe Expr                                         }
           | Native       { _id :: ID, _segments  :: [Expr]                                                                      }
           | NativeCode   { _id :: ID, _code      :: String                                                                      }
           | NativeVar    { _id :: ID, _name      :: String                                                                      }
           | Ref          { _id :: ID, _dst       :: Expr                                                                        }
           | RefType      { _id :: ID, _typeName  :: String   , _name      :: String                                             }
           | Case         { _id :: ID, _expr      :: Expr     , _match     :: [Expr]                                             }
           | Match        { _id :: ID, _pat       :: Pat      , _body      :: [Expr]                                             }
           deriving (Show, Eq, Generic, Read)

data Accessor = VarAccessor { _accName :: String }
              | ConAccessor { _accName :: String }
              deriving (Show, Eq, Generic, Read)
