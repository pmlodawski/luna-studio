---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.CodeGen.HSC.HSC where

import           Flowbox.Prelude                    
import qualified Flowbox.Luna.Data.HAST.Expr      as HExpr
import qualified Flowbox.Luna.Data.HAST.Lit       as HLit
import qualified Flowbox.Luna.Passes.Pass         as Pass
import           Flowbox.Luna.Passes.Pass           (PassMonad)
import           Data.String.Utils                  (join)
import qualified Flowbox.Luna.Data.Source         as Source
import           Flowbox.Luna.Data.Source           (Source(Source))
import           Flowbox.Luna.Data.HAST.Extension   (Extension)

import           Flowbox.System.Log.Logger          


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.HSC.HSC"

type HExpr = HExpr.Expr

type HSCMonad m = PassMonad Pass.NoState m


run :: PassMonad s m => HExpr -> Pass.Result m [Source]
run expr = (Pass.run_ (Pass.Info "HSC") Pass.NoState) (return $ genModule expr)


eol :: String
eol = "\n"


sectionHeader :: String -> String
sectionHeader name = "-- " ++ name ++ " --\n"

genSection :: String -> (a -> String) -> [a] -> String
genSection header generator d = if null d 
    then ""
    else sectionHeader header ++ (join "\n" $ map generator d) ++ "\n\n"


genModule :: HExpr -> [Source]
genModule (HExpr.Module path ext imports newtypes datatypes methods thexpressions) = sources where
    modcode =  genSection    "extensions"     genExt  ext
            ++ sectionHeader "module"         ++ header
            ++ genSection    "imports"        genExpr imports
            ++ genSection    "datatypes"      genExpr datatypes
            ++ genSection    "newtypes"       genExpr newtypes
            ++ genSection    "functions"      genExpr methods
            ++ genSection    "TH expressions" genExpr thexpressions
               where header = "module " ++ join "." path ++ " where" ++ eol ++ eol
    sources = [Source path modcode]


genExt :: Extension -> String
genExt ext = "{-# LANGUAGE " ++ show ext ++ " #-}"


genExpr :: HExpr -> String
genExpr e = case e of
    HExpr.Var      name                   -> name
    HExpr.VarE     name                   -> name
    HExpr.Import   q segments rename      -> "import " 
                                             ++ if q then "qualified " else ""
                                             ++ join "." segments 
                                             ++ case rename of
                                                     Just name -> " as " ++ name
                                                     Nothing   -> ""
    HExpr.DataD    name params cons ders  -> "data " ++ name ++ params' ++ " = " ++ cons' ++ ders' where
                                             params' = if null params then "" else " " ++ join " " params
                                             cons'   = join " | " (map genExpr cons)
                                             ders'   = if null ders then "" else " deriving (" ++ join ", " ders ++ ")"
    HExpr.NewTypeD name params con        -> "newtype " ++ name ++ params' ++ " = " ++ genExpr con where
                                             params' = if null params then "" else " " ++ join " " params
    HExpr.Con      name fields            -> name ++ " { " ++ join ", " (map genExpr fields) ++ " }"
    HExpr.Typed    cls  expr              -> genExpr expr ++ " :: " ++ genExpr cls
    HExpr.TypedP   cls  expr              -> "(" ++ genExpr expr ++ " :: " ++ genExpr cls ++ ")"
    HExpr.Function name signature expr    -> name ++ params ++ " = " ++ genExpr expr where
                                             params = if null signature then ""
                                                      else " " ++ join " " (map genExpr signature)
    HExpr.LetBlock exprs result           -> "let { " ++ join "; " (map genExpr exprs) ++ " } in " ++ genExpr result 
    HExpr.DoBlock  exprs                  -> "do { " ++ join "; " (map genExpr exprs) ++ " }"
    HExpr.Infix    name src dst           -> genExpr src ++ " " ++ name ++ " " ++ genExpr dst
    HExpr.NOP                             -> "NOP"
    HExpr.Assignment src dst              -> genExpr src ++ " <- " ++ genExpr dst
    HExpr.Lit      val                    -> genLit val
    HExpr.Tuple    items                  -> "(" ++ join "," (map genExpr items) ++ ")"
    HExpr.ConE     qname                  -> join "." qname
    HExpr.ConT     name                   -> name
    HExpr.AppT     src dst                -> "(" ++ genExpr src ++ " (" ++ genExpr dst ++ ")" ++ ")" -- for literals, e.g. Pure (1 :: Int)
    HExpr.AppE     src dst                -> "(" ++ genExpr src ++ " " ++ genExpr dst ++ ")"
    --HExpr.NewtypeD 


genLit :: HLit.Lit -> String
genLit lit = case lit of
	HLit.Integer val -> val
	HLit.String  val -> "\"" ++ val   ++ "\""
	HLit.Char    val -> "'"  ++ [val] ++ "'"