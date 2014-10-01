---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Luna.AST.Expr where

import Control.Applicative
import GHC.Generics        (Generic)

import           Flowbox.Generics.Deriving.QShow
import           Flowbox.Prelude                 hiding (Traversal, cons, drop, id)
import           Luna.AST.Common                 (ID)
import qualified Luna.AST.Lit                    as Lit
import qualified Luna.AST.Pat                    as Pat
import           Luna.AST.Type                   (Type)
import qualified Luna.AST.Type                   as Type
import           Luna.Data.Name                  (Name)
import           Luna.AST.Prop                   (HasName)
import qualified Luna.AST.Prop                   as Prop
import qualified Luna.AST.Arg                    as Arg
import           Luna.AST.Arg                    (Arg)


type Lit         = Lit.Lit
type Pat         = Pat.Pat
type Traversal m = (Functor m, Applicative m, Monad m)


data Expr  = NOP          { _id :: ID                                                                                            }
           | Accessor     { _id :: ID, _name      :: String   , _dst       :: Expr                                               }
           | TypeAlias    { _id :: ID, _srcType   :: Type     , _dstType   :: Type                                               }
           | TypeDef      { _id :: ID, _srcType   :: Type     , _dstType   :: Type                                               }
           | App          { _id :: ID, _src       :: Expr     , _args      :: [Arg Expr]                                         }
           -- | AppCons_     { _id :: ID, _args      :: [Expr]                                                                      }
           | Assignment   { _id :: ID, _pat       :: Pat      , _dst       :: Expr                                               }
           | RecordUpdate { _id :: ID, _src       :: Expr     , _selectors :: [String], _expr :: Expr                            }
           | Data         { _id :: ID, _cls       :: Type     , _cons      :: [Expr] , _classes   :: [Expr] , _methods :: [Expr] }
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


shiftArg1 f t1 x = f x t1
shiftArg2 f t1 t2 x = f x t1 t2
shiftArg3 f t1 t2 t3 x = f x t1 t2 t3
shiftArg4 f t1 t2 t3 t4 x = f x t1 t2 t3 t4
shiftArg5 f t1 t2 t3 t4 t5 x = f x t1 t2 t3 t4 t5
shiftArg6 f t1 t2 t3 t4 t5 t6 x = f x t1 t2 t3 t4 t5 t6

var = shiftArg1 Var
function = shiftArg5 Function
app = shiftArg2 App

instance QShow Expr
makeLenses (''Expr)


tupleBuilder id src arg = case src of
    Tuple id items -> Tuple id (items ++ [arg])
    _              -> Tuple id [src, arg]


--callBuilder :: ID -> Expr -> Expr -> Expr
--callBuilder id' src' arg' = case src' of
--    App id'' src'' args' -> App id'' src'' (args' ++ [Arg.Unnamed 0 arg'])
--    _                    -> App id' src' [Arg.Unnamed 0 arg']


--aftermatch :: Expr -> Expr
--aftermatch x = case x of
--    AppCons_ id' (a:as) -> App id' a as
--    _                   -> x


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
    Accessor     id' name' dst'                    -> Accessor     id' name' <$> fexp dst'
    TypeAlias    id' srcType' dstType'             -> TypeAlias    id'       <$> ftype srcType' <*> ftype dstType'
    TypeDef      id' srcType' dstType'             -> TypeDef      id'       <$> ftype srcType' <*> ftype dstType'
    --App          id' src' args'                    -> App          id'       <$> fexp src'      <*> fexpMap args'
    Assignment   id' pat' dst'                     -> Assignment   id'       <$> fpat pat'      <*> fexp dst'
    RecordUpdate id' src' selectors' expr'         -> RecordUpdate id'       <$> fexp src'      <*> pure selectors' <*> fexp expr'
    Data         id' cls' cons' classes' methods'  -> Data         id'       <$> ftype cls'     <*> fexpMap cons' <*> fexpMap classes' <*> fexpMap methods'
    ConD         id' name' fields'                 -> ConD         id' name' <$> fexpMap fields'
    Con          {}                                -> pure e
    Cond         id' cond' success' failure'       -> Cond         id'       <$> fexp cond' <*> fexpMap success' <*> mapM fexpMap failure'
    Field        id' name' cls' value'             -> Field        id' name' <$> ftype cls' <*> fexpMap value'
    Function     id' path' name' inputs' output'
                 body'                             -> Function     id' path' name' <$> fexpMap inputs' <*> ftype output' <*> fexpMap body'
    Lambda       id' inputs' output' body'         -> Lambda       id'             <$> fexpMap inputs' <*> ftype output' <*> fexpMap body'
    Grouped      id' expr'                         -> Grouped      id'       <$> fexp expr'
    Import       id' path' target' rename'         -> Import       id' path' <$> fexp target'  <*> pure rename'
    Infix        id' name' src' dst'               -> Infix        id' name' <$> fexp src'     <*> fexp dst'
    List         id' items'                        -> List         id'       <$> fexpMap items'
    Lit          id' val'                          -> Lit          id'       <$> flit val'
    Tuple        id' items'                        -> Tuple        id'       <$> fexpMap items'
    Typed        id' cls' expr'                    -> Typed        id'       <$> ftype cls' <*> fexp expr'
    Native       id' segments'                     -> Native       id'       <$> fexpMap segments'
    RangeFromTo  id' start' end'                   -> RangeFromTo  id'       <$> fexp start' <*> fexp end'
    RangeFrom    id' start'                        -> RangeFrom    id'       <$> fexp start'
    Case         id' expr' match'                  -> Case         id'       <$> fexp expr' <*> fexpMap match'
    Match        id' pat' body'                    -> Match        id'       <$> fpat pat' <*> fexpMap body'
    ImportNative id' segments'                     -> ImportNative id'       <$> fexpMap segments'
    NativeCode   {}                                -> pure e
    NativeVar    {}                                -> pure e
    Ref          id' dst'                          -> Ref          id'       <$> fexp dst'
    RefType      {}                                -> pure e
    Var          {}                                -> pure e
    Wildcard     {}                                -> pure e
    NOP          {}                                -> pure e
    Arg          id' pat' value'                   -> Arg          id'       <$> fpat pat' <*> fexpMap value'
    where fexpMap = mapM fexp


traverseM_ :: Traversal m => (Expr -> m a) -> (Type -> m b) -> (Pat -> m c) -> (Lit -> m d) -> Expr -> m ()
traverseM_ fexp ftype fpat flit e = case e of
    Accessor     _  _ dst'                         -> drop <* fexp dst'
    TypeAlias    _ srcType' dstType'               -> drop <* ftype srcType' <* ftype dstType'
    TypeDef      _ srcType' dstType'               -> drop <* ftype srcType' <* ftype dstType'
    --App          _  src' args'                     -> drop <* fexp src'  <* fexpMap args'
    Assignment   _  pat' dst'                      -> drop <* fpat pat'  <* fexp dst'
    RecordUpdate _ src' _ expr'                    -> drop <* fexp src'  <* fexp expr'
    Data         _ cls' cons'  classes' methods'   -> drop <* ftype cls' <* fexpMap cons' <* fexpMap classes' <* fexpMap methods'
    ConD         _ _ fields'                       -> drop <* fexpMap fields'
    Cond         _ cond' success' failure'         -> drop <* fexp cond' <* fexpMap success' <* mapM fexpMap failure'
    Con          {}                                -> drop
    Field        _ _ cls' value'                   -> drop <* ftype cls' <* fexpMap value'
    Function     _ _ _ inputs' output' body'       -> drop <* fexpMap inputs' <* ftype output' <* fexpMap body'
    Lambda       _ inputs' output' body'           -> drop <* fexpMap inputs' <* ftype output' <* fexpMap body'
    Grouped      _ expr'                           -> drop <* fexp expr'
    Import       _ _ target' _                     -> drop <* fexp target'
    Infix        _  _ src' dst'                    -> drop <* fexp src'     <* fexp dst'
    List         _  items'                         -> drop <* fexpMap items'
    Lit          _  val'                           -> drop <* flit val'
    Tuple        _  items'                         -> drop <* fexpMap items'
    Typed        _  cls' _expr'                    -> drop <* ftype cls' <* fexp _expr'
    Native       _ segments'                       -> drop <* fexpMap segments'
    RangeFromTo  _ start' end'                     -> drop <* fexp start' <* fexp end'
    RangeFrom    _ start'                          -> drop <* fexp start'
    Case         _ expr' match'                    -> drop <* fexp expr' <* fexpMap match'
    Match        _ pat' body'                      -> drop <* fpat pat'  <* fexpMap body'
    ImportNative _ segments'                       -> drop <* fexpMap segments'
    NativeCode   {}                                -> drop
    NativeVar    {}                                -> drop
    Ref          _ dst'                            -> drop <* fexp dst'
    RefType      {}                                -> drop
    Var          {}                                -> drop
    Wildcard     {}                                -> drop
    NOP          {}                                -> drop
    Arg          _ pat' value'                     -> drop <* fpat pat' <* fexpMap value'
    where drop    = pure ()
          fexpMap = mapM_ fexp


traverseM' :: Traversal m => (Expr -> m Expr) -> Expr -> m Expr
traverseM' fexp = traverseM fexp pure pure pure


traverseM'_ :: Traversal m => (Expr -> m ()) -> Expr -> m ()
traverseM'_ fexp = traverseM_ fexp pure pure pure


traverseMR :: Traversal m => (Expr -> m Expr) -> (Type -> m Type) -> (Pat -> m Pat) -> (Lit -> m Lit) -> Expr -> m Expr
traverseMR fexp ftype fpat flit = tfexp where
    tfexp e = fexp  =<< traverseM tfexp tftype tfpat flit e
    tfpat   = Pat.traverseMR fpat tftype flit
    tftype  = Type.traverseMR ftype


instance HasName Expr where
  name = _name