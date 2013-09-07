---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module Flowbox.Luna.Parser.AST.AST where

import           Flowbox.Luna.Parser.AST.Type       (Type)
import qualified Flowbox.Luna.Parser.AST.Constant as Constant

data Expr  = NOP
           | Import           { paths :: [Expr] }
           | ImportQualified  { path :: Expr, imports :: Expr  }
           | Identifier      String 
           | TypeIdentifier  String
           | Constant        Constant.Constant
           | Assignment      { src :: Expr, dst :: Expr }
           | Tuple           { items :: [Expr] }
           | Interface       { name :: String, body :: [Expr]}
           | Typed           String Expr
           | Path            { segments :: [String] } -- , name::String}
           | Named           { name :: String, item :: Expr }
           | Call            { src :: Expr, args :: [Expr] }
           | CallConstructor { args :: [Expr] }
           | Accessor        { src :: Expr, dst :: Expr }
           | Operator        { name :: String, src :: Expr, dst :: Expr }
           
           | Comment         String
           | Program         { body :: [Expr] }
           | Field           { name :: String, cls :: Type}
           | Lambda          { signature :: Type, body :: [Expr] }
           | Cons            { src :: Expr, args :: [Expr] }
           | Wildcard
           | Function        { name :: String , signature :: Type   , body    :: [Expr] }
           | Class           { cls  :: Type   , fields    :: [Expr] , methods :: [Expr] }
           | Pattern         Expr
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