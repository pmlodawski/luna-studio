---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

module Flowbox.Luna.Passes.CodeGen.HSC.HSC where

import           Flowbox.Prelude                  hiding(cons)                
import qualified Flowbox.Luna.Data.HAST.Expr      as HExpr
import qualified Flowbox.Luna.Data.HAST.Lit       as HLit
import qualified Flowbox.Luna.Passes.Pass         as Pass
import           Flowbox.Luna.Passes.Pass           (Pass)
import           Data.String.Utils                  (join)
import           Flowbox.Luna.Data.Source           (Source(Source))
import           Flowbox.Luna.Data.HAST.Extension   (Extension)

import           Flowbox.System.Log.Logger          


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.HSC.HSC"

type HExpr = HExpr.Expr

type HSCPass result = Pass Pass.NoState result



run :: HExpr -> Pass.Result [Source]
run expr = Pass.run_ (Pass.Info "HSC") Pass.NoState (return $ genModule expr)


eol :: String
eol = "\n"


sectionHeader :: String -> String
sectionHeader name = "-- " ++ name ++ " --\n"

genSection :: String -> (a -> String) -> [a] -> String
genSection header generator d = if null d 
    then ""
    else sectionHeader header ++ (join "\n" $ map generator d) ++ "\n\n"


genModule :: HExpr -> [Source]
genModule (HExpr.Module path ext imports body) = sources where
    modcode =  genSection    "extensions"     genExt  ext
            ++ sectionHeader "module"         ++ header
            ++ genSection    "imports"        genExpr imports
            ++ genSection    "body"           genExpr body
            -- ++ genSection    "newtypes"       genExpr newtypes
            -- ++ genSection    "functions"      genExpr methods
            -- ++ genSection    "TH expressions" genExpr thexpressions
               where header = "module " ++ join "." path ++ " where" ++ eol ++ eol
    sources = [Source path modcode]


genExt :: Extension -> String
genExt ext = "{-# LANGUAGE " ++ show ext ++ " #-}"


data CodeBuilder a = Simple  { code :: a }
                   | Complex { code :: a }

instance Functor CodeBuilder where
    fmap f builder = builder { code = f $ code builder }

instance Applicative CodeBuilder where
    pure a = Simple a
    l <*> r = case l of
        Simple f -> case r of
            Simple  v -> Simple  $ f v
            Complex v -> Complex $ f v
        Complex f -> Complex $ f (code r)



genExpr e = code $ buildExpr e

simplify c = case c of
    Simple {} -> c
    Complex v -> Simple $ "(" ++ v ++ ")"

app a b = Complex (\x y -> x ++ " " ++ y) <*> a <*> (simplify b)

buildExpr :: HExpr -> CodeBuilder String
buildExpr e = case e of
    HExpr.WildP                           -> pure "_"
    HExpr.Var      name                   -> pure name
    HExpr.VarE     name                   -> pure name
    HExpr.VarT     name                   -> pure name
    HExpr.Import   q segments rename      -> pure $ "import " 
                                             ++ if q then "qualified " else ""
                                             ++ join "." segments 
                                             ++ case rename of
                                                     Just name -> " as " ++ name
                                                     Nothing   -> ""
    HExpr.DataD    name params cons ders  -> pure $ "data " ++ name ++ params' ++ " = " ++ cons' ++ ders' 
                                             where params' = if null params then "" else " " ++ join " " params
                                                   cons'   = join " | " (fExpMap cons)
                                                   ders'   = if null ders then "" else " deriving (" ++ sepjoin (map show ders) ++ ")"
    HExpr.InstanceD tp decs               -> pure $ "instance " ++ (code.buildExpr) tp ++ " where { " ++ join "; " (map (code.buildExpr) decs) ++ " }"
    HExpr.NewTypeD name params con        -> pure $ "newtype " ++ name ++ params' ++ " = " ++ (code.buildExpr) con 
                                             where params' = if null params then "" else " " ++ join " " params
    HExpr.Con      name fields            -> pure $ name ++ body
                                             where body = if null fields then "" else " { " ++ sepjoin (fExpMap fields) ++ " }"
    HExpr.RecUpdE  expr name val          -> Complex $ csBuildExpr expr ++ " { " ++ name ++ " = " ++ cBuildExpr val ++ "}"
    HExpr.Typed    cls  expr              -> Complex $ cBuildExpr expr ++ " :: " ++ (code.buildExpr) cls
    HExpr.TypedP   cls  expr              -> Complex $ cBuildExpr expr ++ " :: " ++ (code.buildExpr) cls
    HExpr.TypedE   cls  expr              -> Complex $ cBuildExpr expr ++ " :: " ++ (code.buildExpr) cls
    HExpr.Function name signature expr    -> pure $ name ++ params ++ " = " ++ (code.buildExpr) expr 
                                             where params = if null signature then ""
                                                            else " " ++ join " " (fExpMap signature)
    HExpr.Lambda   signature expr         -> pure $ "(\\" ++ params ++ " -> " ++ (code.simplify.buildExpr) expr ++ ")"
                                             where params = if null signature then ""
                                                            else " " ++ join " " (fsExpMap signature)
    HExpr.LetBlock exprs result           -> pure $ "let { " ++ join "; " (fExpMap exprs) ++ " } in " ++ (code.buildExpr) result 
    HExpr.DoBlock  exprs                  -> pure $ "do { " ++ body ++ " }"
                                             where body = if null exprs then "" else join "; " (fExpMap exprs) ++ ";"
    HExpr.Infix    name src dst           -> Complex $ csBuildExpr src ++ " " ++ name ++ " " ++ csBuildExpr dst
    HExpr.NOP                             -> pure $ "NOP"
    HExpr.Assignment src dst              -> pure $ (code.buildExpr) src ++ " = " ++ (code.buildExpr) dst
    HExpr.Arrow      src dst              -> pure $ (code.buildExpr) src ++ " <- " ++ (code.buildExpr) dst
    HExpr.Lit      val                    -> pure $ genLit val
    HExpr.LitT     val                    -> pure $ genLit val
    HExpr.Tuple    items                  -> if length items == 1 then app (Simple "OneTuple") (buildExpr $ items!!0)
                                                                  else Simple $ "(" ++ sepjoin (map csBuildExpr items) ++ ")"
    --pure $ "(" ++ (if length items == 1 then "OneTuple" else "")
    --                                         ++ sepjoin (fExpMap items) ++ ")"
    HExpr.TupleP   items                  -> pure $ "(" ++ sepjoin (fExpMap items) ++ ")"
    HExpr.ConE     qname                  -> pure $ join "." qname
    HExpr.ConT     name                   -> pure $ name
    HExpr.AppT     src dst                -> app (buildExpr src) (buildExpr dst) --"(" ++ (code.buildExpr) src ++ " (" ++ (code.buildExpr) dst ++ ")" ++ ")" -- for literals, e.g. Pure (1 :: Int)
    HExpr.AppE     src dst                -> app (buildExpr src) (buildExpr dst) --"(" ++ (code.buildExpr) src ++ " " ++ (code.buildExpr) dst ++ ")"
    HExpr.Native   code                   -> pure $ code
    HExpr.ListE    items                  -> pure $ "[" ++ sepjoin (fExpMap items) ++ "]"
    HExpr.Bang     expr                   -> pure $ "--->>>   " ++ (code.buildExpr) expr
    HExpr.THE      expr                   -> pure $ (code.buildExpr) expr
    where sepjoin = join ", "
          fExpMap  = map cBuildExpr
          fsExpMap = map csBuildExpr
          cBuildExpr  = code.buildExpr
          csBuildExpr = code.simplify.buildExpr


genLit :: HLit.Lit -> String
genLit lit = case lit of
    HLit.Integer val -> val
    HLit.Int     val -> val
    HLit.Float   val -> val
    HLit.String  val -> "\"" ++ val   ++ "\""
    HLit.Char    val -> "'"  ++ [val] ++ "'"