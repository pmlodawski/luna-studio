---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Flowbox.Luna.Data.AST.Expr where

import           Control.Applicative
import           Flowbox.Generics.Deriving.QShow
import qualified Flowbox.Luna.Data.AST.Lit       as Lit
import qualified Flowbox.Luna.Data.AST.Pat       as Pat
import           Flowbox.Luna.Data.AST.Type      (Type)
import           Flowbox.Luna.Data.AST.Utils     (ID)
import           Flowbox.Prelude                 hiding (Accessor, Traversal, drop, id)
import           GHC.Generics                    (Generic)



type Lit         = Lit.Lit
type Pat         = Pat.Pat
type Traversal m = (Functor m, Applicative m, Monad m)


data Expr  = NOP         { _id :: ID                                                                                            }
           | Accessor    { _id :: ID, _name      :: String   , _dst       :: Expr                                               }
           | App         { _id :: ID, _src       :: Expr     , _args      :: [Expr]                                             }
           | AppCons_    { _id :: ID, _args      :: [Expr]                                                                      }
           | Assignment  { _id :: ID, _pat       :: Pat      , _dst       :: Expr                                               }
           | Class       { _id :: ID, _cls       :: Type     , _classes   :: [Expr] , _fields    :: [Expr] , _methods :: [Expr] }
           | Con         { _id :: ID, _name      :: String                                                                      }
           | Function    { _id :: ID, _path      :: [String] , _name      :: String , _inputs    :: [Expr] , _output  :: Type   , _body    :: [Expr] }
           | Lambda      { _id :: ID, _inputs    :: [Expr]   , _output    :: Type   , _body      :: [Expr]                      }
           | Import      { _id :: ID, _path      :: [String] , _target    :: Expr   , _rename    :: Maybe String                }
           | Infix       { _id :: ID, _name      :: String   , _src       :: Expr   , _dst       :: Expr                        }
           | List        { _id :: ID, _items     :: [Expr]                                                                      }
           | Lit         { _id :: ID, _lvalue    :: Lit                                                                         }
           | Tuple       { _id :: ID, _items     :: [Expr]                                                                      }
           | Typed       { _id :: ID, _cls       :: Type     , _expr      :: Expr                                               }
           | Var         { _id :: ID, _name      :: String                                                                      }
           | Wildcard    { _id :: ID                                                                                            }
           | RangeFromTo { _id :: ID, _start     :: Expr     , _end       :: Expr                                               }
           | RangeFrom   { _id :: ID, _start     :: Expr                                                                        }
           | Field       { _id :: ID, _name      :: String   , _cls       :: Type   , _value     :: Maybe Expr                  }
           | Arg         { _id :: ID, _pat       :: Pat      , _value     :: Maybe Expr                                         }
           | Native      { _id :: ID, _segments  :: [Expr]                                                                      }
           | NativeCode  { _id :: ID, _code      :: String }
           | NativeVar   { _id :: ID, _name      :: String }
           deriving (Show, Eq, Generic)


instance QShow Expr
makeLenses (''Expr)


callConstructor :: ID -> Expr -> Expr -> Expr
callConstructor id' src' arg' = case src' of
    (AppCons_ id'' args') -> AppCons_ id'' (args' ++ [arg'])
    _                     -> AppCons_ id' (src':[arg'])


aftermatch :: Expr -> Expr
aftermatch x = case x of
    AppCons_ id' (a:as) -> App id' a as
    _                   -> x


addMethod :: Expr -> Expr -> Expr
addMethod method e = e & methods %~ (method:)


addField :: Expr -> Expr -> Expr
addField field e = e & fields %~ (field:)


addClass :: Expr -> Expr -> Expr
addClass ncls e = e & classes %~ (ncls:)


traverseM :: Traversal m => (Expr -> m Expr) -> (Type -> m Type) -> (Pat -> m Pat) -> (Lit -> m Lit) -> Expr -> m Expr
traverseM fexp ftype fpat flit e = case e of
    Accessor    id' name' dst'                     -> Accessor    id' name' <$> fexp dst'
    App         id' src' args'                     -> App         id'       <$> fexp src'  <*> fexpMap args'
    Assignment  id' pat' dst'                      -> Assignment  id'       <$> fpat pat'  <*> fexp dst'
    Class       id' cls' classes' fields' methods' -> Class       id'       <$> ftype cls' <*> fexpMap classes' <*> fexpMap fields' <*> fexpMap methods'
    Con         {}                                 -> pure e
    Field       id' name' cls' value'              -> Field       id' name' <$> ftype cls' <*> fexpMap value'
    Function    id' path' name' inputs' output'
                body'                              -> Function    id' path' name' <$> fexpMap inputs' <*> ftype output' <*> fexpMap body'
    Lambda      id' inputs' output' body'          -> Lambda      id'             <$> fexpMap inputs' <*> ftype output' <*> fexpMap body'
    Import      {}                                 -> pure e
    Infix       id' name' src' dst'                -> Infix       id' name' <$> fexp src'     <*> fexp dst'
    List        id' items'                         -> List        id'       <$> fexpMap items'
    Lit         id' val'                           -> Lit         id'       <$> flit val'
    Tuple       id' items'                         -> Tuple       id'       <$> fexpMap items'
    Typed       id' cls' _expr'                    -> Typed       id'       <$> ftype cls' <*> fexp _expr'
    Native      id' segments'                      -> Native      id'       <$> fexpMap segments'
    RangeFromTo id' start' end'                    -> RangeFromTo id'       <$> fexp start' <*> fexp end'
    RangeFrom   id' start'                         -> RangeFrom   id'       <$> fexp start'
    NativeCode  {}                                 -> pure e
    NativeVar   {}                                 -> pure e
    Var         {}                                 -> pure e
    Wildcard    {}                                 -> pure e
    NOP         {}                                 -> pure e
    AppCons_    {}                                 -> pure e
    Arg         id' pat' value'                    -> Arg         id'       <$> fpat pat' <*> fexpMap value'
    where fexpMap = mapM fexp
          fpatMap = mapM fpat


traverseM_ :: Traversal m => (Expr -> m a) -> (Type -> m b) -> (Pat -> m c) -> (Lit -> m d) -> Expr -> m ()
traverseM_ fexp ftype fpat flit e = case e of
    Accessor    _  name' dst'                      -> drop <* fexp dst'
    App         _  src' args'                      -> drop <* fexp src'  <* fexpMap args'
    Assignment  _  pat' dst'                       -> drop <* fpat pat'  <* fexp dst'
    Class       _  cls' classes' fields' methods'  -> drop <* ftype cls' <* fexpMap classes' <* fexpMap fields' <* fexpMap methods'
    Con         {}                                 -> drop
    Field       _ _ cls' value'                    -> drop <* ftype cls' <* fexpMap value'
    Function    _ _ _ inputs' output' body'        -> drop <* fexpMap inputs' <* ftype output' <* fexpMap body'
    Lambda      _ inputs' output' body'            -> drop <* fexpMap inputs' <* ftype output' <* fexpMap body'
    Import      {}                                 -> drop
    Infix       _  _ src' dst'                     -> drop <* fexp src'     <* fexp dst'
    List        _  items'                          -> drop <* fexpMap items'
    Lit         _  val'                            -> drop <* flit val'
    Tuple       _  items'                          -> drop <* fexpMap items'
    Typed       _  cls' _expr'                      -> drop <* ftype cls' <* fexp _expr'
    Native      _ segments'                        -> drop <* fexpMap segments'
    RangeFromTo _ start' end'                      -> drop <* fexp start' <* fexp end'
    RangeFrom   _ start'                           -> drop <* fexp start'
    NativeCode  {}                                 -> drop
    NativeVar   {}                                 -> drop
    Var         {}                                 -> drop
    Wildcard    {}                                 -> drop
    NOP         {}                                 -> drop
    AppCons_    {}                                 -> drop
    Arg         _ pat' value'                      -> drop <* fpat pat' <* fexpMap value'
    where drop    = pure ()
          fexpMap = mapM_ fexp
          fpatMap = fpatMap


traverseM' :: Traversal m => (Expr -> m Expr) -> Expr -> m Expr
traverseM' fexp e = traverseM fexp pure pure pure e


traverseM'_ :: Traversal m => (Expr -> m ()) -> Expr -> m ()
traverseM'_ fexp e = traverseM_ fexp pure pure pure e
