---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FunctionalDependencies    #-}

module Luna.ASTNew.Expr where

import Flowbox.Prelude

import Control.Applicative
import GHC.Generics        (Generic)

import           Flowbox.Generics.Deriving.QShow

import           Luna.ASTNew.Name (Name, VName, TName, CName, TVName, Named)
import qualified Luna.ASTNew.Name as Name

import           Luna.ASTNew.Decl       (Decl)
import           Luna.ASTNew.Lit        (Lit)
import           Luna.ASTNew.Pat        (Pat, RPat)
import           Luna.ASTNew.Type       (Type, RType)
import           Luna.ASTNew.Native     (Native)
import           Luna.ASTNew.Name.Multi (MultiName)
import           Luna.ASTNew.Arg        (RArg)


type ID = Int



--type Lit         = Lit.Lit
--type Pat         = Pat.Pat
--type Traversal m = (Functor m, Applicative m, Monad m)


type Selector = [VName]

--data Fingered f a = Fingered f a

--type FingeredID = Fingered ID



--type FExpr f t e p l v = Expr (f t) (f e) (f p) (f l) (f v)

--type FingerExpr t e p l v = FExpr FingeredID t e p l v

--type ExprStage1 = FingerExpr String String String String String
----type ExprStage2 = FingerExpr Type   Expr   Pat    Lit    String


----data Def d e t p
----    = Data         { _tname     :: TName      , params   :: [TVName]      , _cons   :: [f (Constructor e)] , _defs   :: [d]              }
----    | Function     { _path      :: Path       , _fname   :: VName         , _inputs :: [f (PatArg e)]      , _output :: t , _body :: e }
----    | TypeAlias    { _srcType   :: t          , _dstType :: t                                                                            }
----    | TypeWrapper  { _srcType   :: t          , _dstType :: t                                                                            }
----    | Import       { _from      :: Maybe Path , _targets :: [ImportTarget]                                                               }
----    -- | TypeFunction { _srcType   :: f Type , _dstType :: f Type  


--data Constructor e = Constructor { _name :: CName, _fields :: [Field e] }

--data Field e = Field { _cls :: Type, _name :: Maybe VName, _value :: Maybe e }


--type Arg p e = Named (PatVal p e)

--data PatVal p e = PatVal { _pat :: p, _value :: Maybe e }


--data Named a = Named   { _arg :: a, _name :: String }
--             | Unnamed { _arg :: a                  }
--             deriving (Show, Eq, Generic, Read)

---- TODO: pythonowe importy
--data ImpTgt = ImpTgt { _path :: Path , _name :: Name, _rename :: Maybe Name } deriving (Show, Eq, Generic, Read)


--data AppArgs e = Seq   [Named e]
--               | Infix e e

--data Expr e t p l v  
--    = Data         { _tname   :: TName      , params    :: [TVName]  , _cons   :: [Constructor e] , _defs   :: [e]              }
--    | Function     { _path    :: Path       , _fname    :: MultiName , _inputs :: [Arg p e]       , _output :: t , _body :: [e] }
--    | TypeAlias    { _dstType :: t          , _srcType  :: t                                                                    }
--    | TypeWrapper  { _dstType :: t          , _srcType  :: t                                                                    }
--    | Import       { _from    :: Maybe Path , _targets  :: [ImpTgt]                                                             }
--    --| TypeFunction { _srcType :: f Type     , _dstType  :: f Type                                                               }
--    | Lambda       { _inputs  :: [Arg p e]  , _output   :: t        , _body   :: [e]                                            }
--    | RecordUpdate { _src     :: e          , _selector :: Selector , _expr   :: e                                              }
--    | App          { _src     :: e          , _args     :: AppArgs                                                              }
--    | Accessor     { _acc     :: Name       , _src      :: e                                                                    }
--    | Typed        { _cls     :: t          , _expr     :: e                                                                    }
--    | Case         { _expr    :: e          , _match    :: [Match p e]                                                          }
--    | Assignment   { _pat     :: p          , _dst      :: e                                                                    }
--    | Con          { _cname   :: CName                                                                                          }
--    | Grouped      { _expr    :: e                                                                                              }
--    | List         { _items   :: [e]                                                                                            }
--    | Lit          { _lvalue  :: l                                                                                              }
--    | Tuple        { _items   :: [e]                                                                                            }
--    | Var          { _value   :: v                                                                                              }
--    | Ref          { _dst     :: e                                                                                              }
--    --| Block        { _exprs   :: [e]                                                                                          }
--    | Native       (Native e)
--    | Wildcard
--    | NOP
--    deriving (Show, Eq, Generic, Read)

---------------------------


--data PatVal f = PatVal { _pat :: RPat f, _value :: Maybe (RExpr f) }

--type Arg f = Named (PatVal f)


-- TODO: pythonowe importy


data App e = Seq   [Named e VName]
         | Infix e e





type R f a = f (a f)

type RExpr f a = f (Expr f a)

type ExprArg f a = RArg f (Expr f a)

data Expr f a
    = Lambda      { _inputs  :: [ExprArg f a] , _output   :: RType f         , _body   :: [RExpr f a] }
    | RecUpdt     { _src     :: RExpr f a     , _selector :: Selector        , _expr   :: RExpr f a   }
    | App         { _src     :: RExpr f a     , _args     :: App (RExpr f a)                          }
    | Case        { _expr    :: RExpr f a     , _match    :: [RMatch f a]                             }
    | Typed       { _cls     :: RType f       , _expr     :: RExpr f a                                }
    | Assignment  { _dst     :: RPat  f       , _src      :: RExpr f a                                }
    | Accessor    { _acc     :: Name          , _src      :: RExpr f a                                }
    | Ref         { _ref     :: RExpr f a                                                             }
    | List        { _items   :: [RExpr f a]                                                           }
    | Tuple       { _items   :: [RExpr f a]                                                           }
    | Grouped     { _expr    :: RExpr f a                                                             }
    | Lit         { _lit     :: f Lit                                                                 }
    | Cons        { _cname   :: CName                                                                 }
    | Var         { _ident   :: a                                                                     }
    | Native      (Native (RExpr f a))
    | Wildcard
    deriving (Generic)



data Match f a = Match { _matchPat :: RPat f, _matchBody :: [RExpr f a] }  
type RMatch f a = f (Match f a)














--newtype MuExpr f d t p l v    = MuExpr (f (MuExpr f t p l v) t p l (d (MuD d t p) t p) v)

--type RecExpr v = MuExpr Expr RecType RecPat RecLit v


--data Def d t e p
--    = Data         { _tname     :: TName  , params   :: [TVName] , _cons   :: [f (Constructor e)] , _defs   :: [d]                  }
--    | Function     { _path      :: Path   , _fname   :: VName    , _inputs :: [f (PatArg p e)]      , _output :: t , _body :: f e }
--    | TypeAlias    { _srcType   :: t , _dstType :: t                                                                    }
--    | TypeWrapper  { _srcType   :: t , _dstType :: t                                                                    }
--    -- | TypeFunction { _srcType   :: f Type , _dstType :: f Type  


--data Def f e
--    = Data         { _tname     :: TName  , params   :: [TVName] , _cons   :: [f (Constructor e)] , _defs   :: [f Def]                  }
--    | Function     { _path      :: Path   , _fname   :: VName    , _inputs :: [f (PatArg e)]      , _output :: f Type , _body :: f e }
--    | TypeAlias    { _srcType   :: f Type , _dstType :: f Type                                                                    }
--    | TypeWrapper  { _srcType   :: f Type , _dstType :: f Type                                                                    }
--    -- | TypeFunction { _srcType   :: f Type , _dstType :: f Type                                                                    }



--data Expr f
--    = Lambda       { _inputs :: f (Expr f) , _output    :: f Type             , _body :: f (Expr f) }
--    | RecordUpdate { _src    :: f (Expr f) , _selector  :: Selector           , _expr :: f (Expr f) }
--    | Infix        { _vname  :: VName      , _src       :: f (Expr f)         , _dst  :: f (Expr f) }
--    | Import       { _from   :: Maybe Path , _targets   :: [ImportTarget]                           }
--    | Accessor     { _acc    :: Name       , _src       :: f (Expr f)                               }
--    | App          { _src    :: f (Expr f) , _args      :: [f (Arg (Expr f))]                       }
--    | Typed        { _cls    :: f Type     , _expr      :: f (Expr f)                               }
--    | Case         { _expr   :: f (Expr f) , _match     :: f (Expr f)                               }
--    | Match        { _pat    :: f Pat      , _body      :: f (Expr f)                               }
--    | Assignment   { _pat    :: f Pat      , _dst       :: f (Expr f)                               }
--    | Con          { _cname  :: CName                                                               }
--    | Grouped      { _expr   :: f (Expr f)                                                          }
--    | Lit          { _lvalue :: f Lit                                                               }
--    | Tuple        { _items  :: [f (Expr f)]                                                        }
--    | Var          { _value  :: String                                                              }
--    | Ref          { _dst    :: f (Expr f)                                                          }
--    | Block        { _exprs  :: [f (Expr f)]                                                        }
--    | List         List
--    | Native       Native
--    | Def          (Def f (Expr f))
--    | Wildcard
--    | NOP
--    deriving (Show, Eq, Generic, Read)


--data List e = ElementList  { _elements :: [e]         }
--            | RangeList    { _begin    :: e, end :: e }
--            | InfiniteList { _begin    :: e           }
--            deriving (Show, Eq, Generic, Read)





--data Lit = Char    { _char :: Prelude.Char   }
--         | String  { _str  :: Prelude.String }
--         | Number  { _num  :: Number         }
--         deriving (Show, Eq, Generic, Read)


            --newtype Mu f = Mu (f (Mu f))

            ----type X t p l v = MuE Expr t p l v

            --newtype MuE f t p l v = MuE (f t (MuE f t p l v) p l v)
            --newtype MuT f         = MuT (f (MuT f))


            ----class Rec a b | a -> b where
            ----    rec :: a -> b


            --newtype Rec6 f t1 t2 t3 t4 t5 = Rec6 (f (Rec6 f t1 t2 t3 t4 t5) t1 t2 t3 t4 t5)
            --newtype Rec5 f t1 t2 t3 t4    = Rec5 (f (Rec5 f t1 t2 t3 t4) t1 t2 t3 t4)
            --newtype Rec4 f t1 t2 t3       = Rec4 (f (Rec4 f t1 t2 t3) t1 t2 t3)
            --newtype Rec3 f t1 t2          = Rec3 (f (Rec3 f t1 t2) t1 t2)
            --newtype Rec2 f t1             = Rec2 (f (Rec2 f t1) t1)
            --newtype Rec1 f                = Rec1 (f (Rec1 f))
            --newtype Rec0 f                = Rec0 f


            --data Def d t e p = Def'
            --data Type t      = Type'

            --data Pat p l     = Pat'
            --data Lit         = Lit'

            ---- data Expr e t p l v  


            ----RecE e = RecE (e (RecT t) (RecE e) (RecP p) (RecL l) (RecV v))







            ----a :: Expr () (Expr () () () () String) () () ()

            --type RecLit    = Rec0 Lit
            --type RecType   = Rec1 Type
            --type RecPat    = Rec2 Pat RecLit
            --type RecExpr v = Rec5 Expr RecType RecPat RecLit v

            --type RecDef  v = Rec4 Def (RecExpr v) RecType RecPat

            --a :: RecExpr String
            --a = Rec5 $ Grouped (Rec5 $ Var "x")


            ----data A = A B

            ----data B = B A 


            --data A a = A a
            --data B b = B b
            --         | B'


            --newtype RM f g = RM (f (RM g f))

            --b :: RM B A
            --b = RM (B (RM (A (RM (B')))))





--data Range = RangeType RangeBorders



--data RangeBorders = Left
--                  | Both







--data Type = Unknown
--          | Var      { _name     :: TVName                          }
--          | Tuple    { _items    :: [Type]                          }
--          | List     { _item     :: Type                            }
--          | Data     { _name     :: TName    , _params  :: [TVName] }
--          | Module   { _name     :: TName    , _path    :: [TName]  }
--          | Function { _inputs   :: [Type]   , _output  :: Type     }
--          | App      { _src      :: Type     , _args    :: [Type]   }
--          -- ??? | Con      { _segments :: [String]                        }
--          deriving (Show, Eq, Generic, Read)








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