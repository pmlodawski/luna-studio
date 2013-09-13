---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}

module Flowbox.Luna.AST.Expr where

import           Flowbox.Prelude             
import           Flowbox.Luna.AST.Type       (Type)
import qualified Flowbox.Luna.AST.Lit      as Lit
import qualified Flowbox.Luna.AST.Pat      as Pat
import           Data.Typeable
import           Flowbox.Generics.Deriving.QShow
import           GHC.Generics

type Lit = Lit.Lit
type Pat = Pat.Pat

data Expr  = NOP
           | Import          { segments  :: [String] , name      :: String                                                                                   }
           | Var             { name      :: String                                                                                                           }
           | TypeVar         { name      :: String                                                                                                           }
           | Lit             { value     :: Lit                                                                                                              }
           | Assignment      { src       :: Expr     , dst       :: Expr                                                                                     }
           | Tuple           { items     :: [Expr]                                                                                                           }
           | Interface       { name      :: String   , body      :: [Expr]                                                                                   }
           | Typed           { cls       :: Type     , expr      :: Expr                                                                                     }
           | Path            { segments  :: [String]                                                                                                         }
           | Call            { src       :: Expr     , args      :: [Expr]                                                                                   }
           | CallConstructor { args      :: [Expr]                                                                                                           }
           | Accessor        { src       :: Expr     , dst       :: Expr                                                                                     }
           | Infix           { name      :: String   , src       :: Expr   , dst     :: Expr                                                                 }                                                               
           | Comment         { txt       :: String                                                                                                           }
           | Class           { cls       :: Type     , classes   :: [Expr] , fields    :: [Expr] , methods :: [Expr]                                         }
           | Module          { cls       :: Type     , imports   :: [Expr] , classes   :: [Expr] , fields  :: [Expr] , methods :: [Expr] , modules :: [Expr] }
           | Field           { name      :: String   , cls       :: Type                                                                                     }
           | Lambda          { signature :: Type     , body      :: [Expr]                                                                                   }
           | Cons            { src       :: Expr     , args      :: [Expr]                                                                                   }
           | Function        { name      :: String   , signature :: Type   , body    :: [Expr]                                                               }
           | Pattern         { pat       :: Pat                                                                                                              }
           deriving (Show, Eq, Generic)


instance QShow Expr


callConstructor :: Expr -> Expr -> Expr
callConstructor src' arg' = case src' of
    call @ CallConstructor{} -> call { args = args call ++ [arg'] }
    _                        -> CallConstructor $ src':[arg']


consConstructor :: Expr -> Expr -> Expr
consConstructor src' dst' = case dst' of
    CallConstructor args' -> Cons src' args'
    _                     -> Cons src' [dst']

--mkClass :: String -> Expr
--mkClass name' = Class name' [] []


aftermatch :: Expr -> Expr
aftermatch x = case x of
    CallConstructor (a:as) -> Call a as
    _                      -> x


addMethod :: Expr -> Expr -> Expr
addMethod method e = e { methods = method : methods e }


addField :: Expr -> Expr -> Expr
addField field e = e { fields = field : fields e }

addClass :: Expr -> Expr -> Expr
addClass ncls e = e { classes = ncls : classes e }


addImport :: Expr -> Expr -> Expr
addImport imp e = e { imports = imp : imports e }