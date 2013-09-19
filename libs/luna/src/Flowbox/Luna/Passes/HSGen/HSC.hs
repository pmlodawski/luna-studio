---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.HSGen.HSC where

import           Flowbox.Prelude                          
import qualified Flowbox.Luna.Passes.HSGen.AST.Expr     as HExpr
import qualified Flowbox.Luna.Passes.HSGen.AST.Lit      as HLit
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



run :: PassMonad s m => HExpr.Expr -> Pass.Result m String
run expr = (Pass.run_ Pass.NoState) (return $ genExpr expr)


eol :: String
eol = "\n"


genSection :: String -> (a -> String) -> [a] -> String
genSection header generator d = if null d 
    then ""
    else "-- " ++ header ++ "\n" ++ (join "\n" $ map generator d) ++ "\n\n"


genExpr :: HExpr.Expr -> String
genExpr expr = case expr of
    HExpr.Var      name                   -> name
    HExpr.VarE     name                   -> name
    HExpr.Import   segments name          -> "import qualified " ++ join "." segments ++ " as " ++ name
    HExpr.Module   path imports datatypes 
                   methods                -> header 
                                          ++ genSection "imports"   genExpr imports
                                          ++ genSection "datatypes" genExpr datatypes
                                          ++ genSection "methods"   genExpr methods
                                             where header = "module " ++ join "." path ++ " where" ++ eol
    HExpr.DataType name params cons       -> "data " ++ name ++ params' ++ " = " ++ join " | " (map genExpr cons)
                                             where params' = if null params then "" else " " ++ join " " params
    HExpr.Con      name fields            -> name ++ " { " ++ join ", " (map genExpr fields) ++ " }"
    HExpr.Typed    cls  expr              -> genExpr expr ++ " :: " ++ genExpr cls
    HExpr.TypedP   cls  expr              -> "(" ++ genExpr expr ++ " :: " ++ genExpr cls ++ ")"
    HExpr.Function name signature expr    -> name ++ " " ++ join " " (map genExpr signature) ++ " = " ++ genExpr expr
    HExpr.LetBlock exprs result           -> "let { " ++ join "; " (map genExpr exprs) ++ " } in " ++ genExpr result 
    HExpr.DoBlock  exprs                  -> "do { " ++ join "; " (map genExpr exprs) ++ " }"
    HExpr.Infix    name src dst           -> genExpr src ++ " " ++ name ++ " " ++ genExpr dst
    HExpr.NOP                             -> "NOP"
    HExpr.Assignment src dst              -> genExpr src ++ " <- " ++ genExpr dst
    HExpr.Lit      val                    -> genLit val
    HExpr.Tuple    items                  -> "(" ++ join "," (map genExpr items) ++ ")"
    HExpr.ConE     qname                  -> join "." qname
    HExpr.ConT     name                   -> name
    HExpr.AppT     src dst                -> "(" ++ genExpr src ++ " " ++ genExpr dst ++ ")"
    HExpr.AppE     src dst                -> "(" ++ genExpr src ++ " " ++ genExpr dst ++ ")"



genLit :: HLit.Lit -> String
genLit lit = case lit of
	HLit.Integer val -> val
	HLit.String  val -> "\"" ++ val   ++ "\""
	HLit.Char    val -> "'"  ++ [val] ++ "'"