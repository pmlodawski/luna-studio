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

import           Luna.ASTNew.Name (Name, VName, TName, CName, TVName)
import qualified Luna.ASTNew.Name as Name

import           Luna.ASTNew.Decl       (Decl)
import           Luna.ASTNew.Lit        (Lit)
import           Luna.ASTNew.Pat        (Pat, RPat)
import           Luna.ASTNew.Type       (Type, RType)
import           Luna.ASTNew.Native     (Native)
import           Luna.ASTNew.Name.Multi (MultiName)
import           Luna.ASTNew.Arg        (Arg)


type Selector = [VName]

data App e = Seq   [Named e VName]
           | Infix e e


data Named a n = Named   a n
               | Unnamed a  
               deriving (Show, Eq, Generic, Read)

type RExpr f a   = f (Expr f a)
type ExprApp f a = f (App (RExpr f a))
type ExprArg f a = f (Arg f (Expr f a))
type SubDecl f a = f (Decl f (RExpr f a))

data Expr f a
    = Lambda      { _inputs  :: [ExprArg f a] , _output   :: RType f      , _body   :: [RExpr f a] }
    | RecUpdt     { _src     :: RExpr f a     , _selector :: Selector     , _expr   :: RExpr f a   }
    | App         { _src     :: RExpr f a     , _args     :: ExprApp f a                           }
    | Case        { _expr    :: RExpr f a     , _match    :: [RMatch f a]                          }
    | Typed       { _cls     :: RType f       , _expr     :: RExpr f a                             }
    | Assignment  { _dst     :: RPat  f       , _src      :: RExpr f a                             }
    | Accessor    { _acc     :: Name          , _src      :: RExpr f a                             }
    | Ref         { _ref     :: RExpr f a                                                          }
    | List        { _items   :: [RExpr f a]                                                        }
    | Tuple       { _items   :: [RExpr f a]                                                        }
    | Grouped     { _expr    :: RExpr f a                                                          }
    | Cons        { _cname   :: CName                                                              }
    | Decl        { _decl    :: SubDecl f a                                                        }
    | Lit         { _lit     :: f Lit                                                              }
    | Native      { _native  :: Native (RExpr f a)                                                 }
    | Var         { _ident   :: a                                                                  }
    | Wildcard
    deriving (Generic)



data Match f a = Match { _matchPat :: RPat f, _matchBody :: [RExpr f a] }  
type RMatch f a = f (Match f a)


