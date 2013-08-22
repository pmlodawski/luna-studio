---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module Flowbox.Luna.Parser.AST.AST where

import qualified Flowbox.Luna.Parser.AST.Constant as Constant

import Debug.Trace

import Control.Exception
instance Exception [Char]


data Expr = NOP
          | Import           { paths :: [Expr] }
          | ImportQualified  { path :: Expr, imports :: Expr  }
          | Identifier      String 
          | TypeIdentifier  String
          | Constant        Constant.Constant
          | Assignment      { src :: Expr, dst :: Expr }
          | Tuple           { items :: [Expr] }
          | Function        { name :: String, body :: [Expr]} --, signature :: [Expr], body :: [Expr]}
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

          deriving (Show, Eq)


--callConstructor src' arg' = Call src' [arg']

callConstructor src' arg' = case src' of
    call @ CallConstructor{} -> call { args = args call ++ [arg'] }
    _             -> CallConstructor src' [arg']


--callConstructor src' arg' = Call src' [arg']


mkFunction :: String -> Expr
mkFunction name'  = Function name' []

mkClass :: String -> Expr
mkClass name' = Class name' [] []

--mkImports a = Imports
--mkImports a = trace(show a)Imports 

--addSubExprs sub expr = 
--  case expr of
--      f@Function{} -> f {body = sub:body f}

setBody :: [Expr] -> Expr -> Expr
setBody exprs el = case trace(show exprs)el of
    Function{} -> el { body = exprs }
    Class   {} -> el { body = exprs }
    Import  {} -> el
    _          -> throw $ "Cannot define body of '" ++ (show el) ++ "'"


aftermatch x = case x of
    CallConstructor src args -> Call src args
    --id@Identifier{} -> Call id []
    _               -> x