---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}

module Luna.Pass.CodeGen.HSC.HSC where

import           Data.String.Utils        (join)
import           Flowbox.Prelude          hiding (cons)
import qualified Luna.Data.HAST.Comment   as HComment
import qualified Luna.Data.HAST.Expr      as HExpr
import           Luna.Data.HAST.Extension (Extension)
import qualified Luna.Data.HAST.Lit       as HLit
import           Luna.Data.Source         (Source (Source))
import           Luna.Pass.Pass           (Pass)
import qualified Luna.Pass.Pass           as Pass

import Flowbox.System.Log.Logger


logger :: Logger
logger = getLogger $(moduleName)

type HExpr = HExpr.Expr
type HComment = HComment.Comment

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
    else sectionHeader header ++ join "\n" (map generator d) ++ "\n\n"


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
    pure = Simple
    l <*> r = case l of
        Simple f -> case r of
            Simple  v -> Simple  $ f v
            Complex v -> Complex $ f v
        Complex f -> Complex $ f (code r)


genExpr :: HExpr -> String
genExpr e = code $ buildExpr e

simplify :: CodeBuilder String -> CodeBuilder String
simplify c = case c of
    Simple {} -> c
    Complex v -> Simple $ "(" ++ v ++ ")"


app :: CodeBuilder String -> CodeBuilder String -> CodeBuilder String
app a b = Complex (\x y -> x ++ " " ++ y) <*> a <*> simplify b

buildExpr :: HExpr -> CodeBuilder String
buildExpr e = case e of
    HExpr.WildP                           -> pure "_"
    HExpr.RecWildP                        -> pure "{}"
    HExpr.Var      name                   -> pure name
    HExpr.VarE     name                   -> pure name
    HExpr.VarT     name                   -> pure name
    HExpr.ImportNative code               -> pure $ "import " ++ code
    HExpr.Import   q segments rename      -> pure $ "import "
                                             ++ if q then "qualified " else ""
                                             ++ join "." segments
                                             ++ case rename of
                                                     Just name -> " as " ++ name
                                                     Nothing   -> ""
    HExpr.DataD    name params cons ders  -> pure $ "data " ++ name ++ " " ++ unwords params ++ " = " ++ cons' ++ ders'
                                             where cons'   = join " | " (fExpMap cons)
                                                   ders'   = if null ders then "" else " deriving (" ++ sepjoin (map show ders) ++ ")"
    HExpr.InstanceD tp decs               -> pure $ "instance " ++ (code.buildExpr) tp ++ " where { " ++ join "; " (map (code.buildExpr) decs) ++ " }"
    HExpr.NewTypeD name params con        -> pure $ "newtype " ++ name ++ " " ++ (unwords . fsExpMap) params ++ " = " ++ (code.buildExpr) con
    HExpr.Con      name fields            -> pure $ name ++ unwords ("":fsExpMap fields)
    --HExpr.CondE    cond sucess failure    -> Complex $ "if " ++ csBuildExpr cond ++ " then " ++ (code.buildDoBlock) sucess ++ " else " ++ (code.buildDoBlock) failure
    HExpr.CondE    cond sucess failure    -> Complex $ "ifThenElse' " ++ csBuildExpr cond ++ (code.simplify.buildDoBlock) sucess ++ (code.simplify.buildDoBlock) failure
    HExpr.RecUpdE  expr name val          -> Complex $ csBuildExpr expr ++ " { " ++ name ++ " = " ++ cBuildExpr val ++ "}"
    HExpr.Typed    cls  expr              -> Complex $ cBuildExpr expr ++ " :: " ++ cBuildExpr cls
    HExpr.TypedP   cls  expr              -> Complex $ cBuildExpr expr ++ " :: " ++ cBuildExpr cls
    HExpr.TypedE   cls  expr              -> Complex $ cBuildExpr expr ++ " :: " ++ cBuildExpr cls
    HExpr.TySynD   name params dstType    -> Complex $ "type " ++ name ++ " " ++ unwords (fsExpMap params) ++ " = " ++ (code.buildExpr) dstType
    HExpr.Function name signature expr    -> pure $ name ++ unwords ("":fExpMap signature) ++ " = " ++ (code.buildExpr) expr
    HExpr.Lambda   signature expr         -> pure $ "(\\" ++ unwords ("":fsExpMap signature) ++ " -> " ++ (code.simplify.buildExpr) expr ++ ")"
    HExpr.LetBlock exprs result           -> pure $ "let { " ++ join "; " (fExpMap exprs) ++ " } in " ++ (code.buildExpr) result
    HExpr.LetExpr  expr                   -> pure $ "let " ++ cBuildExpr expr
    HExpr.DoBlock  exprs                  -> buildDoBlock exprs
    HExpr.Infix    name src dst           -> Complex $ csBuildExpr src ++ " " ++ name ++ " " ++ csBuildExpr dst
    HExpr.NOP                             -> pure $ "nop"
    HExpr.Assignment src dst              -> pure $ (code.buildExpr) src ++ " = " ++ (code.buildExpr) dst
    HExpr.Arrow      src dst              -> pure $ (code.buildExpr) src ++ " <- " ++ (code.buildExpr) dst
    HExpr.Lit      val                    -> pure $ genLit val
    HExpr.LitT     val                    -> pure $ genLit val
    HExpr.Tuple    items                  -> if length items == 1 then app (Simple "OneTuple") (buildExpr $ head items)
                                                                  else Simple $ "(" ++ sepjoin (map csBuildExpr items) ++ ")"
    --pure $ "(" ++ (if length items == 1 then "OneTuple" else "")
    --                                         ++ sepjoin (fExpMap items) ++ ")"
    HExpr.TupleP   items                  -> pure $ "(" ++ sepjoin (fExpMap items) ++ ")"
    HExpr.ConE     qname                  -> pure $ join "." qname
    HExpr.ConT     name                   -> pure $ name
    HExpr.ConP     name                   -> pure $ name
    HExpr.AppT     src dst                -> app (buildExpr src) (buildExpr dst) --"(" ++ (code.buildExpr) src ++ " (" ++ (code.buildExpr) dst ++ ")" ++ ")" -- for literals, e.g. Pure (1 :: Int)
    HExpr.AppE     src dst                -> app (buildExpr src) (buildExpr dst) --"(" ++ (code.buildExpr) src ++ " " ++ (code.buildExpr) dst ++ ")"
    HExpr.AppP     src dst                -> app (buildExpr src) (buildExpr dst)
    HExpr.Native   natCode                -> pure $ natCode
    HExpr.ListE    items                  -> pure $ "[" ++ sepjoin (fExpMap items) ++ "]"
    HExpr.Bang     expr                   -> pure $ "--->>>   " ++ (code.buildExpr) expr
    HExpr.THE      expr                   -> pure $ "$(" ++ (code.buildExpr) expr ++ ")"
    HExpr.CaseE    expr matches           -> Complex $ "case " ++ (code.buildExpr) expr ++ " of {" ++ buildBody matches ++ "}"
    HExpr.Match    pat matchBody          -> Complex $ (code.buildExpr) pat ++ " -> " ++ (code.buildExpr) matchBody
    HExpr.Comment  comment                -> buildComment comment
    HExpr.ViewP    name dst               -> pure $ "(" ++ name ++ " -> " ++ (code.buildExpr) dst ++ ")"
    --_                                     ->
    where sepjoin     = join ", "


buildComment :: HComment -> CodeBuilder String
buildComment comment = pure $ case comment of
    HComment.H1 str -> mkSpace 2 ++ "-- " ++ replicate 67 '=' ++ "\n-- " ++ str ++ "\n" ++ "-- " ++ replicate 67 '='
    HComment.H2 str -> mkSpace 1 ++ "-- ====== " ++ str ++ " ====== --"
    HComment.H3 str -> mkSpace 1 ++ "-- ------ " ++ str ++ " ------ --"
    HComment.H4 str -> "-- --- " ++ str ++ " --- --"
    HComment.H5 str -> "-- " ++ str
    where mkSpace n = replicate n '\n'

fExpMap :: [HExpr] -> [String]
fExpMap     = map cBuildExpr

fsExpMap :: [HExpr] -> [String]
fsExpMap    = map csBuildExpr

cBuildExpr :: HExpr -> String
cBuildExpr  = code.buildExpr

csBuildExpr :: HExpr -> String
csBuildExpr = code.simplify.buildExpr

buildDoBlock :: [HExpr] -> CodeBuilder String
buildDoBlock exprs = Complex $ "do { " ++ buildBody exprs ++ " }"

buildBody :: [HExpr] -> String
buildBody exprs = if null exprs then "" else join "; " (fExpMap exprs) ++ ";"


genLit :: HLit.Lit -> String
genLit lit = case lit of
    HLit.Integer val -> escapeNegative val
    HLit.Int     val -> escapeNegative val
    HLit.Float   val -> escapeNegative val
    HLit.String  val -> "\"" ++ val   ++ "\""
    HLit.Char    val -> "'"  ++ [val] ++ "'"
    where escapeNegative num@('-':_) = '(' : num ++ ")"
          escapeNegative num         = num
