---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module Flowbox.Luna.AST.AST where

import           Flowbox.Prelude             
import           Flowbox.Luna.AST.Type       (Type)
import qualified Flowbox.Luna.AST.Constant as Constant

type Constant = Constant.Constant

data Expr  = NOP
           | Import          { segments  :: [String] , name      :: String                                                                                   }
           | Identifier      { name      :: String                                                                                                           }
           | TypeIdentifier  { name      :: String                                                                                                           }
           | Constant        { value     :: Constant                                                                                                         }
           | Assignment      { src       :: Expr     , dst       :: Expr                                                                                     }
           | Tuple           { items     :: [Expr]                                                                                                           }
           | Interface       { name      :: String   , body      :: [Expr]                                                                                   }
           | Typed           { name      :: String   , src       :: Expr                                                                                     }
           | Path            { segments  :: [String]                                                                                                         }
           | Call            { src       :: Expr     , args      :: [Expr]                                                                                   }
           | CallConstructor { args      :: [Expr]                                                                                                           }
           | Accessor        { src       :: Expr     , dst       :: Expr                                                                                     }
           | Operator        { name      :: String   , src       :: Expr   , dst     :: Expr                                                                 }                                                               
           | Comment         { txt       :: String                                                                                                           }
           | Class           { cls       :: Type     , classes   :: [Expr] , fields    :: [Expr] , methods :: [Expr]                                         }
           | Module          { cls       :: Type     , imports   :: [Expr] , classes   :: [Expr] , fields  :: [Expr] , methods :: [Expr] , modules :: [Expr] }
           | Field           { name      :: String   , cls       :: Type                                                                                     }
           | Lambda          { signature :: Type     , body      :: [Expr]                                                                                   }
           | Cons            { src       :: Expr     , args      :: [Expr]                                                                                   }
           | Function        { name      :: String   , signature :: Type   , body    :: [Expr]                                                               }
           | Pattern         { expr      :: Expr                                                                                                             }
           | Wildcard
           deriving (Show, Eq)


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