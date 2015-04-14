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

module Luna.Syntax.Expr where

import Flowbox.Prelude

import Control.Applicative
import GHC.Generics        (Generic)

import           Flowbox.Generics.Deriving.QShow

import Luna.Syntax.Name         (VNameP, TNameP, CNameP, TVNameP, NameBaseP)
import Luna.Syntax.Decl         (Decl)
import Luna.Syntax.Lit          (LLit)
import Luna.Syntax.Pat          (Pat, LPat)
import Luna.Syntax.Type         (Type, LType, LMeta)
import Luna.Syntax.Native       (Native)
import Luna.Syntax.Arg          (LArg)
import Luna.Syntax.Label        (Label(Label))
import Luna.Syntax.Name.Pattern (NamePat(NamePat), Segment(Segment))
import Data.Text.Lazy           (Text)


type Selector = [VNameP]

type LExpr   a v = Label a (Expr a v)
type ExprArg a v = LArg  a (LExpr a v)
type SubDecl a v = Label a (Decl a (LExpr a v))


type ArgName = Text 

data Expr a v
    = Lambda      { _inputs  :: [ExprArg a v] , _output    :: Maybe (LType a) , _body   :: [LExpr a v] }
    | RecUpd      { _vname   :: VNameP        , _fieldUpds :: [FieldUpd a v]                           }
    | Case        { _expr    :: LExpr a v     , _match     :: [LMatch a v]                             }
    | Typed       { _cls     :: LType a       , _expr      :: LExpr a v                                }
    | Assignment  { _dst     :: LPat  a       , _src       :: LExpr a v                                }
    | Accessor    { _acc     :: NameBaseP     , _src       :: LExpr a v                                }
    | Curry       { _expr    :: LExpr a v                                                              }
    | Meta        { _meta    :: LMeta a                                                                }
    | Tuple       { _items   :: [LExpr a v]                                                            }
    | Grouped     { _expr    :: LExpr a v                                                              }
    | Cons        { _cname   :: CNameP                                                                 }
    | Decl        { _decl    :: SubDecl a v                                                            }
    | Lit         { _lit     :: LLit a                                                                 }
    | Native      { _native  :: Native (LExpr a v)                                                     }
    | Var         { _ident   :: Variable v                                                             }
    | List        (List (LExpr a v))
    | App         { _exprApp :: ExprApp a v }
    | Wildcard
    deriving (Show, Generic, Eq, Read)


data FieldUpd a v = FieldUpd { _selector :: Selector, _updExpr :: LExpr a v } deriving (Show, Generic, Eq, Read)

data AppArg e = AppArg (Maybe ArgName) e deriving (Show, Generic, Eq, Read)

data Variable v = Variable VNameP v deriving (Show, Generic, Eq, Read)

type ExprApp a v = NamePat (LExpr a v) (AppArg (LExpr a v))

app src args          = App $ NamePat Nothing    (Segment src args) []
appInfix src pfx args = App $ NamePat (Just pfx) (Segment src args) []

var n v = Var $ Variable n v

unnamed = AppArg Nothing



data Match  a v = Match { _matchPat :: LPat a, _matchBody :: [LExpr a v] } deriving (Show, Generic, Eq, Read)
type LMatch a v = Label a (Match a v)


data List e = SeqList [e]
            | RangeList (Sequence e)
            deriving (Show, Generic, Eq, Read)

type LList a e = Label a (List e)

data Sequence a = Linear    a   (Maybe a)
                | Geometric a a (Maybe a)
                deriving (Show, Generic, Read, Eq)

makeLenses ''Expr


