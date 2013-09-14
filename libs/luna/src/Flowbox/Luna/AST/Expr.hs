---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, DeriveGeneric, NoMonomorphismRestriction #-}

module Flowbox.Luna.AST.Expr where

import           Flowbox.Prelude           hiding(id)      
import           Flowbox.Luna.AST.Type       (Type)
import qualified Flowbox.Luna.AST.Lit      as Lit
import qualified Flowbox.Luna.AST.Pat      as Pat
import           Flowbox.Luna.AST.Utils      (ID)
import           Data.Typeable
import           Flowbox.Generics.Deriving.QShow
import           GHC.Generics
import           Control.Applicative  

import           Text.Parsec                       hiding (parse, many, optional, (<|>))

type Lit = Lit.Lit
type Pat = Pat.Pat

data Expr  = NOP        { id :: ID                                                                                                                                }
           | Import     { id :: ID, segments  :: [String] , name      :: String                                                                                   }
           | Var        { id :: ID, name      :: String                                                                                                           }
           | Lit        { id :: ID, value     :: Lit                                                                                                              }
           | Assignment { id :: ID, src       :: Expr     , dst       :: Expr                                                                                     }
           | Tuple      { id :: ID, items     :: [Expr]                                                                                                           }
           | Typed      { id :: ID, cls       :: Type     , expr      :: Expr                                                                                     }
           | App        { id :: ID, src       :: Expr     , args      :: [Expr]                                                                                   }
           | AppCons_   { id :: ID, args      :: [Expr]                                                                                                           }
           | Accessor   { id :: ID, src       :: Expr     , dst       :: Expr                                                                                     }
           | Infix      { id :: ID, name      :: String   , src       :: Expr   , dst     :: Expr                                                                 }                                                               
           | Class      { id :: ID, cls       :: Type     , classes   :: [Expr] , fields    :: [Expr] , methods :: [Expr]                                         }
           | Module     { id :: ID, cls       :: Type     , imports   :: [Expr] , classes   :: [Expr] , fields  :: [Expr] , methods :: [Expr] , modules :: [Expr] }
           | Field      { id :: ID, name      :: String   , cls       :: Type                                                                                     }
           | Lambda     { id :: ID, signature :: [Pat]    , body      :: [Expr]                                                                                   }
           | Cons       { id :: ID, segments  :: [String]                                                                                                         }
           | Function   { id :: ID, name      :: String   , signature :: [Pat]   , body    :: [Expr]                                                              }
           | List       { id :: ID, items     :: [Expr]                                                                                                           }
           | Pattern    { id :: ID, pat       :: Pat                                                                                                              }
           deriving (Show, Eq, Generic)


instance QShow Expr


callConstructor :: ID -> Expr -> Expr -> Expr
callConstructor id' src' arg' = case src' of
    (AppCons_ id'' args') -> AppCons_ id'' (args' ++ [arg'])
    _                     -> AppCons_ id' (src':[arg'])



aftermatch :: Expr -> Expr
aftermatch x = case x of
    AppCons_ id' (a:as) -> App id' a as
    _                   -> x


addMethod :: Expr -> Expr -> Expr
addMethod method e = e { methods = method : methods e }


addField :: Expr -> Expr -> Expr
addField field e = e { fields = field : fields e }

addClass :: Expr -> Expr -> Expr
addClass ncls e = e { classes = ncls : classes e }


addImport :: Expr -> Expr -> Expr
addImport imp e = e { imports = imp : imports e }