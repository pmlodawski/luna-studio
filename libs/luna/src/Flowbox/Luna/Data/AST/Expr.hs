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
import qualified Flowbox.Luna.Data.AST.Type      as Type
import           Flowbox.Luna.Data.AST.Type      (Type)
import           Flowbox.Luna.Data.AST.Utils     (ID)
import           Flowbox.Prelude                 hiding (Accessor, Traversal, drop, id, cons)
import           GHC.Generics                    (Generic)



type Lit         = Lit.Lit
type Pat         = Pat.Pat
type Traversal m = (Functor m, Applicative m, Monad m)


data Expr  = NOP          { _id :: ID                                                                                            }
           | Accessor     { _id :: ID, _name      :: String   , _dst       :: Expr                                               }
           | App          { _id :: ID, _src       :: Expr     , _args      :: [Expr]                                             }
           | AppCons_     { _id :: ID, _args      :: [Expr]                                                                      }
           | Assignment   { _id :: ID, _pat       :: Pat      , _dst       :: Expr                                               }
           | RecordUpdate { _id :: ID, _name      :: String   , _selectors :: [String], _expr :: Expr}
           | Data         { _id :: ID, _cls       :: Type     , _cons      :: [Expr] , _classes   :: [Expr] , _methods :: [Expr] }
           | ConD         { _id :: ID, _name      :: String   , _fields    :: [Expr]                                             }
           | Con          { _id :: ID, _name      :: String                                                                      }
           | Function     { _id :: ID, _path      :: [String] , _name      :: String , _inputs    :: [Expr] , _output  :: Type   , _body    :: [Expr] }
           | Lambda       { _id :: ID, _inputs    :: [Expr]   , _output    :: Type   , _body      :: [Expr]                      }
           | Import       { _id :: ID, _path      :: [String] , _target    :: Expr   , _rename    :: Maybe String                }
           | Infix        { _id :: ID, _name      :: String   , _src       :: Expr   , _dst       :: Expr                        }
           | List         { _id :: ID, _items     :: [Expr]                                                                      }
           | Lit          { _id :: ID, _lvalue    :: Lit                                                                         }
           | Tuple        { _id :: ID, _items     :: [Expr]                                                                      }
           | Typed        { _id :: ID, _cls       :: Type     , _expr      :: Expr                                               }
           | Var          { _id :: ID, _name      :: String                                                                      }
           | Wildcard     { _id :: ID                                                                                            }
           | RangeFromTo  { _id :: ID, _start     :: Expr     , _end       :: Expr                                               }
           | RangeFrom    { _id :: ID, _start     :: Expr                                                                        }
           | Field        { _id :: ID, _name      :: String   , _cls       :: Type   , _value     :: Maybe Expr                  }
           | Arg          { _id :: ID, _pat       :: Pat      , _value     :: Maybe Expr                                         }
           | Native       { _id :: ID, _segments  :: [Expr]                                                                      }
           | NativeCode   { _id :: ID, _code      :: String                                                                      }
           | NativeVar    { _id :: ID, _name      :: String                                                                      }
           | Case         { _id :: ID, _expr      :: Expr     , _match     :: [Expr]                                            }
           | Match        { _id :: ID, _pat       :: Pat      , _body      :: [Expr]                                             }
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

addFieldDC :: Expr -> Expr -> Expr
addFieldDC field e = e & cons .~ addField field defc : cons' where
    defc:cons' = e ^. cons


afterData :: Expr -> Expr
afterData d = nd where
    dcons = d ^. cons
    defc  = last dcons
    ncons = if length dcons == 1
                then [defc & name .~ (nd ^. (cls. Type.name))]
                else init dcons
    nd = d & cons .~ ncons


addClass :: Expr -> Expr -> Expr
addClass ncls e = e & classes %~ (ncls:)

addCon :: Expr -> Expr -> Expr
addCon ncon e = e & cons %~ (ncon:)


traverseM :: Traversal m => (Expr -> m Expr) -> (Type -> m Type) -> (Pat -> m Pat) -> (Lit -> m Lit) -> Expr -> m Expr
traverseM fexp ftype fpat flit e = case e of
    Accessor     id' name' dst'                      -> Accessor     id' name' <$> fexp dst'
    App          id' src' args'                      -> App          id'       <$> fexp src'  <*> fexpMap args'
    Assignment   id' pat' dst'                       -> Assignment   id'       <$> fpat pat'  <*> fexp dst'
    RecordUpdate id' name' selectors' expr'          -> RecordUpdate id' name' selectors' <$> fexp expr'
    Data         id' cls' cons' classes' methods'    -> Data         id'       <$> ftype cls' <*> fexpMap cons' <*> fexpMap classes' <*> fexpMap methods'
    ConD         id' name' fields'                   -> ConD         id' name' <$> fexpMap fields'
    Con          {}                                  -> pure e       
    Field        id' name' cls' value'               -> Field        id' name' <$> ftype cls' <*> fexpMap value'
    Function     id' path' name' inputs' output'                      
                 body'                               -> Function     id' path' name' <$> fexpMap inputs' <*> ftype output' <*> fexpMap body'
    Lambda       id' inputs' output' body'           -> Lambda       id'             <$> fexpMap inputs' <*> ftype output' <*> fexpMap body'
    Import       id' path' target' rename'           -> Import       id' path' <$> fexp target'  <*> pure rename'
    Infix        id' name' src' dst'                 -> Infix        id' name' <$> fexp src'     <*> fexp dst'
    List         id' items'                          -> List         id'       <$> fexpMap items'
    Lit          id' val'                            -> Lit          id'       <$> flit val'
    Tuple        id' items'                          -> Tuple        id'       <$> fexpMap items'
    Typed        id' cls' expr'                      -> Typed        id'       <$> ftype cls' <*> fexp expr'
    Native       id' segments'                       -> Native       id'       <$> fexpMap segments'
    RangeFromTo  id' start' end'                     -> RangeFromTo  id'       <$> fexp start' <*> fexp end'
    RangeFrom    id' start'                          -> RangeFrom    id'       <$> fexp start'
    Case         id' expr' match'                    -> Case         id'       <$> fexp expr' <*> fexpMap match'
    Match        id' pat' body'                      -> Match        id'       <$> fpat pat' <*> fexpMap body'
    NativeCode   {}                                  -> pure e
    NativeVar    {}                                  -> pure e
    Var          {}                                  -> pure e
    Wildcard     {}                                  -> pure e
    NOP          {}                                  -> pure e
    AppCons_     {}                                  -> pure e
    Arg          id' pat' value'                     -> Arg          id'       <$> fpat pat' <*> fexpMap value'
    where fexpMap = mapM fexp


traverseM_ :: Traversal m => (Expr -> m a) -> (Type -> m b) -> (Pat -> m c) -> (Lit -> m d) -> Expr -> m ()
traverseM_ fexp ftype fpat flit e = case e of
    Accessor     _  _ dst'                           -> drop <* fexp dst'
    App          _  src' args'                       -> drop <* fexp src'  <* fexpMap args'
    Assignment   _  pat' dst'                        -> drop <* fpat pat'  <* fexp dst'
    RecordUpdate _ _ _ expr'                         -> drop <* fexp expr'
    Data         _ cls' cons'  classes' methods'     -> drop <* ftype cls' <* fexpMap cons' <* fexpMap classes' <* fexpMap methods'
    ConD         _ _ fields'                         -> drop <* fexpMap fields'
    Con          {}                                  -> drop
    Field        _ _ cls' value'                     -> drop <* ftype cls' <* fexpMap value'
    Function     _ _ _ inputs' output' body'         -> drop <* fexpMap inputs' <* ftype output' <* fexpMap body'
    Lambda       _ inputs' output' body'             -> drop <* fexpMap inputs' <* ftype output' <* fexpMap body'
    Import       _ _ target' _                       -> drop <* fexp target'
    Infix        _  _ src' dst'                      -> drop <* fexp src'     <* fexp dst'
    List         _  items'                           -> drop <* fexpMap items'
    Lit          _  val'                             -> drop <* flit val'
    Tuple        _  items'                           -> drop <* fexpMap items'
    Typed        _  cls' _expr'                      -> drop <* ftype cls' <* fexp _expr'
    Native       _ segments'                         -> drop <* fexpMap segments'
    RangeFromTo  _ start' end'                       -> drop <* fexp start' <* fexp end'
    RangeFrom    _ start'                            -> drop <* fexp start'
    Case         _ expr' match'                      -> drop <* fexp expr' <* fexpMap match'
    Match        _ pat' body'                        -> drop <* fpat pat'  <* fexpMap body'
    NativeCode   {}                                  -> drop
    NativeVar    {}                                  -> drop
    Var          {}                                  -> drop
    Wildcard     {}                                  -> drop
    NOP          {}                                  -> drop
    AppCons_     {}                                  -> drop
    Arg          _ pat' value'                       -> drop <* fpat pat' <* fexpMap value'
    where drop    = pure ()
          fexpMap = mapM_ fexp


traverseM' :: Traversal m => (Expr -> m Expr) -> Expr -> m Expr
traverseM' fexp e = traverseM fexp pure pure pure e


traverseM'_ :: Traversal m => (Expr -> m ()) -> Expr -> m ()
traverseM'_ fexp e = traverseM_ fexp pure pure pure e
