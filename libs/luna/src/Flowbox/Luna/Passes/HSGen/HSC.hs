---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.HSGen.HSC where

import           Flowbox.Prelude                          
import qualified Flowbox.Luna.Passes.HSGen.AST.Expr     as HAST
import qualified Flowbox.Luna.Passes.HSGen.AST.Constant as Constant
import qualified Flowbox.Luna.Passes.HSGen.AST.Module   as Module
import qualified Flowbox.Luna.Passes.HSGen.AST.DataType as DataType
import qualified Flowbox.Luna.Passes.HSGen.AST.Cons     as Cons
import qualified Flowbox.Luna.Passes.HSGen.GenState     as GenState
import           Flowbox.Luna.Passes.HSGen.GenState       (GenState)
import qualified Flowbox.Luna.Passes.Pass               as Pass
import           Flowbox.Luna.Passes.Pass                 (PassMonad)
import           Data.String.Utils                        (join)

import           Control.Applicative                      

import           Debug.Trace                              

import           Control.Monad.Trans.Maybe                
import           Control.Monad.Trans.Either               
import           Data.Maybe                               (fromJust)

import qualified Flowbox.System.Log.Logger              as Logger
import           Flowbox.System.Log.Logger                
import qualified Flowbox.System.Log.LogEntry            as LogEntry


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.HSGen.HSC"

type HSCMonad m = PassMonad Pass.NoState m



run :: PassMonad s m => HAST.Expr -> Pass.Result m String
run expr = (Pass.runM Pass.NoState) (return $ genCode expr)


eol :: String
eol = "\n"


genSection :: String -> (a -> String) -> [a] -> String
genSection header generator d = if null d 
    then ""
    else "-- " ++ header ++ "\n" ++ (join "\n" $ map generator d) ++ "\n\n"


genCode :: HAST.Expr -> String
genCode expr = case expr of
    HAST.Var      name                 -> name
    HAST.Import   segments name        -> "import qualified " ++ join "." segments ++ " as " ++ name
    HAST.Module path imports datatypes 
                methods                -> header 
                                       ++ genSection "imports"   genCode imports
                                       ++ genSection "datatypes" genCode datatypes
                                       ++ genSection "methods"   genCode methods
                                          where header = "module " ++ join "." path ++ " where" ++ eol
    HAST.DataType name params cons     -> "data " ++ name ++ params' ++ " = " ++ join " | " (map genCode cons)
                                          where params' = if null params then "" else " " ++ join " " params
    HAST.Cons     name fields          -> name ++ " { " ++ join ", " (map genCode fields) ++ " }"
    HAST.Typed    name expr            -> genCode expr ++ " :: " ++ name
    HAST.Function name signature expr  -> name ++ " " ++ join " " (map genCode signature) ++ " = " ++ genCode expr
    HAST.LetBlock exprs result         -> "let{ " ++ join ";" (map genCode exprs) ++ " } in " ++ genCode result 
    HAST.Operator name src dst         -> genCode src ++ " " ++ name ++ " " ++ genCode dst
    HAST.NOP                           -> "NOP"