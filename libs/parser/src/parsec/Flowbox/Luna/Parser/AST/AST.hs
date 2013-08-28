---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module Flowbox.Luna.Parser.AST.AST where

import qualified Flowbox.Luna.Parser.AST.Constant as Constant

data Expr = NOP
          | Import           { paths :: [Expr] }
          | ImportQualified  { path :: Expr, imports :: Expr  }
          | Identifier      String 
          | TypeIdentifier  String
          | Constant        Constant.Constant
          | Assignment      { src :: Expr, dst :: Expr }
          | Tuple           { items :: [Expr] }
          | Function        { name :: String, signature :: [Expr], body :: [Expr]}
          | Lambda          { signature :: [Expr], body :: [Expr] }
          | Class           { name :: String, params :: [String], body :: [Expr]}
          | Interface       { name :: String, body :: [Expr]}
          | Typed           String Expr
          | Path            { segments :: [String] } -- , name::String}
          | Named           { name :: String, item :: Expr }
          | Call            { src :: Expr, args :: [Expr] }
          | CallConstructor { src :: Expr, args :: [Expr] }
          | Accessor        { src :: Expr, dst :: Expr }
          | Operator        { name :: String, src :: Expr, dst :: Expr }
          | Comment         String
          | Program         { body :: [Expr] }

          deriving (Show, Eq)


callConstructor :: Expr -> Expr -> Expr
callConstructor src' arg' = case src' of
    call @ CallConstructor{} -> call { args = args call ++ [arg'] }
    _             -> CallConstructor src' [arg']


mkClass :: String -> Expr
mkClass name' = Class name' [] []


aftermatch :: Expr -> Expr
aftermatch x = case x of
    CallConstructor src' args' -> Call src' args'
    _               -> x