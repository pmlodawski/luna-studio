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

module Luna.ASTNew.Expr where

--import Control.Applicative
--import GHC.Generics        (Generic)

--import           Flowbox.Generics.Deriving.QShow
--import           Flowbox.Prelude                 hiding (Traversal, cons, drop, id)
--import           Luna.AST.Common                 (ID)
--import qualified Luna.AST.Lit                    as Lit
--import qualified Luna.AST.Pat                    as Pat
--import           Luna.AST.Type                   (Type)
--import qualified Luna.AST.Type                   as Type
--import           Luna.AST.Name                   (Name)
--import           Luna.AST.Prop                   (HasName)
--import qualified Luna.AST.Prop                   as Prop
--import qualified Luna.AST.Arg                    as Arg
--import           Luna.AST.Arg                    (Arg)


--type Lit         = Lit.Lit
--type Pat         = Pat.Pat
--type Traversal m = (Functor m, Applicative m, Monad m)


--type Path = [String]
--type Selector = [String]

--newtype Fingered a = Fingered ID a


--type FingerExpr v = FExppr Fingered v

--type FExpr f v = Expr (f Type) (f Expr) (f Pat) (f Lit) v

--type FExpr f t e p l v = Expr (f t) (f e) (f p) (f l) (f v)

--type FingerExpr = FExpr Fingered

--type ExprStage1 = FingerExpr String String String String String


--data Def t e p 
--    = Data         { _cls       :: t      , _cons      :: [Constructor t e]        , _classes   :: e                   , _methods :: e }
--    | Function     { _path      :: Path   , _fname     :: Name     , _inputs    :: [PatArg p e] , _output  :: t   , _body    :: e }
--    | TypeDef      { _srcType   :: t      , _dstType   :: t                                                            }
--    | TypeAlias    { _srcType   :: t      , _dstType   :: t                                                            }

--data Expr t e p l v  
--    = Lambda       { _inputs    :: e      , _output    :: t        , _body      :: e                                   }
--    | RecordUpdate { _src       :: e      , _selectors :: Selector , _expr      :: e                                   }
--    | Import       { _path      :: Path   , _targets   :: [ImportTarget]                                               }
--    | Infix        { _name      :: String , _src       :: e        , _dst       :: e                                   }
--    | Accessor     { _name      :: String , _src       :: e                                                            }
--    | RefType      { _typeName  :: String , _name      :: String                                                       }
--    | App          { _src       :: e      , _args      :: [Arg e]                                                      }
--    | Typed        { _cls       :: t      , _expr      :: e                                                            }
--    | Case         { _expr      :: e      , _match     :: e                                                            }
--    | Match        { _pat       :: p      , _body      :: e                                                            }
--    | Assignment   { _pat       :: p      , _dst       :: e                                                            }
--    | Con          { _name      :: String                                                                              }
--    | Grouped      { _expr      :: e                                                                                   }
--    | List         { _items     :: [e]                                                                                   }
--    | Lit          { _lvalue    :: l                                                                                   }
--    | Tuple        { _items     :: e                                                                                   }
--    | Var          { _name      :: v                                                                                   }
--    | Ref          { _dst       :: e                                                                                   }
--    | Block        { _exprs     :: [e]                                                                                 }
--    | Range        Range
--    | Native       Native
--    | Def          Def
--    | Wildcard
--    | NOP
--    deriving (Show, Eq, Generic, Read)


--data ImportTarget = ImportTarget { _path :: Path , _rename :: Maybe String }

--data Arg a = Named   { _id :: ID, _name :: String, _arg :: a }
--           | Unnamed { _id :: ID, _arg :: a                  }
--           deriving (Show, Eq, Generic, Read)

--type PatArg p e = Arg (PatVal p e)

--data PatVal p e = PatVal { _pat       :: p      , _value     :: Maybe e }


--data Range = RangeType RangeBorders

--data Constructor t e = Constructor { _name :: String , _fields :: [Field t e] }                                                          }

--data Field t e = Field { _name :: String, _cls :: t, _value :: Maybe e }

--data RangeBorders = Left
--                  | Both

--data Native e = Code   { _code :: String }
--              | Var    { _name :: String }
--              | Data   { _cls  :: t, _cons :: e, _classes :: e, _methods :: e }
--              | Import { _segments :: e }


--class TraversalM a where
--    traverseM :: a -> m a

--shiftArg1 f t1 x = f x t1
--shiftArg2 f t1 t2 x = f x t1 t2
--shiftArg3 f t1 t2 t3 x = f x t1 t2 t3
--shiftArg4 f t1 t2 t3 t4 x = f x t1 t2 t3 t4
--shiftArg5 f t1 t2 t3 t4 t5 x = f x t1 t2 t3 t4 t5
--shiftArg6 f t1 t2 t3 t4 t5 t6 x = f x t1 t2 t3 t4 t5 t6

--var = shiftArg1 Var
--function = shiftArg5 Function
--app = shiftArg2 App

--instance QShow Expr
--makeLenses (''Expr)


--tupleBuilder id src arg = case src of
--    Tuple id items -> Tuple id (items ++ [arg])
--    _              -> Tuple id [src, arg]


----callBuilder :: ID -> Expr -> Expr -> Expr
----callBuilder id' src' arg' = case src' of
----    App id'' src'' args' -> App id'' src'' (args' ++ [Arg.Unnamed 0 arg'])
----    _                    -> App id' src' [Arg.Unnamed 0 arg']


----aftermatch :: Expr -> Expr
----aftermatch x = case x of
----    AppCons_ id' (a:as) -> App id' a as
----    _                   -> x


--addMethod :: Expr -> Expr -> Expr
--addMethod method e = e & methods %~ (method:)


--addField :: Expr -> Expr -> Expr
--addField field e = e & fields %~ (field:)

--addFieldDC :: Expr -> Expr -> Expr
--addFieldDC field e = e & cons .~ addField field defc : cons' where
--    defc:cons' = e ^. cons

--addClass :: Expr -> Expr -> Expr
--addClass ncls e = e & classes %~ (ncls:)

--addCon :: Expr -> Expr -> Expr
--addCon ncon e = e & cons %~ (ncon:)


--argMapM f a = case a of
--    Arg.Unnamed id      arg -> fmap (Arg.Unnamed id)      $ f arg
--    Arg.Named   id name arg -> fmap (Arg.Named   id name) $ f arg


--traverseM :: Traversal m => (Expr -> m Expr) -> (Type -> m Type) -> (Pat -> m Pat) -> (Lit -> m Lit) -> Expr -> m Expr
--traverseM fexp ftype fpat flit e = case e of
--    Accessor     id' name' dst'                    -> Accessor     id' name' <$> fexp dst'
--    TypeAlias    id' srcType' dstType'             -> TypeAlias    id'       <$> ftype srcType' <*> ftype dstType'
--    TypeDef      id' srcType' dstType'             -> TypeDef      id'       <$> ftype srcType' <*> ftype dstType'
--    App          id' src' args'                    -> App          id'       <$> fexp src'      <*> mapM (argMapM fexp) args'
--    Assignment   id' pat' dst'                     -> Assignment   id'       <$> fpat pat'      <*> fexp dst'
--    RecordUpdate id' src' selectors' expr'         -> RecordUpdate id'       <$> fexp src'      <*> pure selectors' <*> fexp expr'
--    Data         id' cls' cons' classes' methods'  -> Data         id'       <$> ftype cls'     <*> fexpMap cons' <*> fexpMap classes' <*> fexpMap methods'
--    DataNative   id' cls' cons' classes' methods'  -> DataNative   id'       <$> ftype cls'     <*> fexpMap cons' <*> fexpMap classes' <*> fexpMap methods'
--    ConD         id' name' fields'                 -> ConD         id' name' <$> fexpMap fields'
--    Con          {}                                -> pure e
--    Cond         id' cond' success' failure'       -> Cond         id'       <$> fexp cond' <*> fexpMap success' <*> mapM fexpMap failure'
--    Field        id' name' cls' value'             -> Field        id' name' <$> ftype cls' <*> fexpMap value'
--    Function     id' path' name' inputs' output'
--                 body'                             -> Function     id' path' name' <$> fexpMap inputs' <*> ftype output' <*> fexpMap body'
--    Lambda       id' inputs' output' body'         -> Lambda       id'             <$> fexpMap inputs' <*> ftype output' <*> fexpMap body'
--    Grouped      id' expr'                         -> Grouped      id'       <$> fexp expr'
--    Import       id' path' target' rename'         -> Import       id' path' <$> fexp target'  <*> pure rename'
--    Infix        id' name' src' dst'               -> Infix        id' name' <$> fexp src'     <*> fexp dst'
--    List         id' items'                        -> List         id'       <$> fexpMap items'
--    Lit          id' val'                          -> Lit          id'       <$> flit val'
--    Tuple        id' items'                        -> Tuple        id'       <$> fexpMap items'
--    Typed        id' cls' expr'                    -> Typed        id'       <$> ftype cls' <*> fexp expr'
--    Native       id' segments'                     -> Native       id'       <$> fexpMap segments'
--    RangeFromTo  id' start' end'                   -> RangeFromTo  id'       <$> fexp start' <*> fexp end'
--    RangeFrom    id' start'                        -> RangeFrom    id'       <$> fexp start'
--    Case         id' expr' match'                  -> Case         id'       <$> fexp expr' <*> fexpMap match'
--    Match        id' pat' body'                    -> Match        id'       <$> fpat pat' <*> fexpMap body'
--    ImportNative id' segments'                     -> ImportNative id'       <$> fexpMap segments'
--    NativeCode   {}                                -> pure e
--    NativeVar    {}                                -> pure e
--    Ref          id' dst'                          -> Ref          id'       <$> fexp dst'
--    RefType      {}                                -> pure e
--    Var          {}                                -> pure e
--    Wildcard     {}                                -> pure e
--    NOP          {}                                -> pure e
--    Arg          id' pat' value'                   -> Arg          id'       <$> fpat pat' <*> fexpMap value'
--    where fexpMap = mapM fexp


--traverseM_ :: Traversal m => (Expr -> m a) -> (Type -> m b) -> (Pat -> m c) -> (Lit -> m d) -> Expr -> m ()
--traverseM_ fexp ftype fpat flit e = case e of
--    Accessor     _  _ dst'                         -> drop <* fexp dst'
--    TypeAlias    _ srcType' dstType'               -> drop <* ftype srcType' <* ftype dstType'
--    TypeDef      _ srcType' dstType'               -> drop <* ftype srcType' <* ftype dstType'
--    App          _  src' args'                     -> drop <* fexp src'  <* mapM_ (argMapM fexp) args'
--    Assignment   _  pat' dst'                      -> drop <* fpat pat'  <* fexp dst'
--    RecordUpdate _ src' _ expr'                    -> drop <* fexp src'  <* fexp expr'
--    Data         _ cls' cons'  classes' methods'   -> drop <* ftype cls' <* fexpMap cons' <* fexpMap classes' <* fexpMap methods'
--    DataNative   _ cls' cons'  classes' methods'   -> drop <* ftype cls' <* fexpMap cons' <* fexpMap classes' <* fexpMap methods'
--    ConD         _ _ fields'                       -> drop <* fexpMap fields'
--    Cond         _ cond' success' failure'         -> drop <* fexp cond' <* fexpMap success' <* mapM fexpMap failure'
--    Con          {}                                -> drop
--    Field        _ _ cls' value'                   -> drop <* ftype cls' <* fexpMap value'
--    Function     _ _ _ inputs' output' body'       -> drop <* fexpMap inputs' <* ftype output' <* fexpMap body'
--    Lambda       _ inputs' output' body'           -> drop <* fexpMap inputs' <* ftype output' <* fexpMap body'
--    Grouped      _ expr'                           -> drop <* fexp expr'
--    Import       _ _ target' _                     -> drop <* fexp target'
--    Infix        _  _ src' dst'                    -> drop <* fexp src'     <* fexp dst'
--    List         _  items'                         -> drop <* fexpMap items'
--    Lit          _  val'                           -> drop <* flit val'
--    Tuple        _  items'                         -> drop <* fexpMap items'
--    Typed        _  cls' _expr'                    -> drop <* ftype cls' <* fexp _expr'
--    Native       _ segments'                       -> drop <* fexpMap segments'
--    RangeFromTo  _ start' end'                     -> drop <* fexp start' <* fexp end'
--    RangeFrom    _ start'                          -> drop <* fexp start'
--    Case         _ expr' match'                    -> drop <* fexp expr' <* fexpMap match'
--    Match        _ pat' body'                      -> drop <* fpat pat'  <* fexpMap body'
--    ImportNative _ segments'                       -> drop <* fexpMap segments'
--    NativeCode   {}                                -> drop
--    NativeVar    {}                                -> drop
--    Ref          _ dst'                            -> drop <* fexp dst'
--    RefType      {}                                -> drop
--    Var          {}                                -> drop
--    Wildcard     {}                                -> drop
--    NOP          {}                                -> drop
--    Arg          _ pat' value'                     -> drop <* fpat pat' <* fexpMap value'
--    where drop    = pure ()
--          fexpMap = mapM_ fexp


--traverseM' :: Traversal m => (Expr -> m Expr) -> Expr -> m Expr
--traverseM' fexp = traverseM fexp pure pure pure


--traverseM'_ :: Traversal m => (Expr -> m ()) -> Expr -> m ()
--traverseM'_ fexp = traverseM_ fexp pure pure pure


--traverseMR :: Traversal m => (Expr -> m Expr) -> (Type -> m Type) -> (Pat -> m Pat) -> (Lit -> m Lit) -> Expr -> m Expr
--traverseMR fexp ftype fpat flit = tfexp where
--    tfexp e = fexp  =<< traverseM tfexp tftype tfpat flit e
--    tfpat   = Pat.traverseMR fpat tftype flit
--    tftype  = Type.traverseMR ftype


--instance HasName Expr where
--  name = _name