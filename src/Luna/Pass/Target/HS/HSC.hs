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
{-# LANGUAGE OverloadedStrings         #-}

module Luna.Pass.Target.HS.HSC where

import           Flowbox.Prelude          hiding (cons, simple)
import qualified Luna.Data.HAST.Comment   as HComment
import qualified Luna.Data.HAST.Expr      as HExpr
import           Luna.Data.HAST.Extension (Extension)
import qualified Luna.Data.HAST.Lit       as HLit
import           Luna.Data.Source         (Source (Source))
import           Luna.Pass.Pass           (Pass)
import qualified Luna.Pass.Pass           as Pass
import           Luna.Pass                    (Pass(Pass), PassMonad)

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Builder as Text
import           Data.Text.Lazy.Builder   (toLazyText, fromLazyText)

import           Data.Text.CodeBuilder    (Generator, CodeType, CodeBuilder, generate, sgenerate, simple, complex, genmap, sgenmap, simplify)

----------------------------------------------------------------------
-- Basic types
----------------------------------------------------------------------

data HASTGen = HASTGen

type PassResult m   = PassMonad () m
type HExpr          = HExpr.Expr
type HPragma        = HExpr.Pragma
type HComment       = HComment.Comment

------------------------------------------------------------------------
-- Pass functions
------------------------------------------------------------------------

pass :: Monad m => Pass () (HExpr -> PassResult m Text)
pass = Pass "HASTGen" "Haskell AST generator" () (return . genModule)

genModule :: HExpr -> Text
genModule (HExpr.Module path ext imports body) = toLazyText modcode where
    modcode =  genSection    "extensions"     genExt  ext
            <> sectionHeader "module"         <> header
            <> genSection    "imports"        generate imports
            <> genSection    "body"           generate body
    header = "module " <> mjoin "." (fmap fromLazyText path) <> " where" <> eol <> eol

genExt :: Extension -> Text.Builder
genExt ext = "{-# LANGUAGE " <> show' ext <> " #-}"

genSection :: Text.Builder -> (a -> Text.Builder) -> [a] -> Text.Builder
genSection header generator d = if null d
    then ""
    else sectionHeader header <> mjoin "\n"  (map generator d) <> "\n\n"


------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------

eol :: Text.Builder
eol = "\n"

sectionHeader :: Text.Builder -> Text.Builder
sectionHeader name = "-- " <> name <> " --\n"

app :: CodeBuilder a => Text.Builder -> CodeType Text.Builder -> a
app a b = complex (a <> " " <> simplify b)

spaceJoin :: (Monoid a, IsString a) => [a] -> a
spaceJoin = mjoin " "

buildDoBlock :: CodeBuilder a => [HExpr] -> a
buildDoBlock exprs = complex $ "do { " <> buildBody exprs <> " }"

buildBody :: (IsString a, CodeBuilder a, Monoid a) => [HExpr] -> a
buildBody exprs = if null exprs then "" else mjoin "; " (genmap exprs) <> ";"

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Generator HExpr where
    generate = \case
        HExpr.WildP                           -> simple  "_"
        HExpr.RecWildP                        -> simple  "{}"
        HExpr.Var      name                   -> simple  $ convert name
        HExpr.VarE     name                   -> simple  $ convert name
        HExpr.VarT     name                   -> simple  $ convert name
        HExpr.ImportNative code               -> simple  $ "import " <> convert code
        HExpr.InstanceD tp decs               -> simple  $ "instance " <> generate tp <> " where { " <> mjoin "; " (genmap decs) <> " }"
        HExpr.TypeInstance tp expr            -> simple  $ "type instance " <> generate tp <> " = " <> generate expr
        HExpr.NewTypeD name params con        -> simple  $ "newtype " <> convert name <> " " <> (spaceJoin . sgenmap) params <> " = " <> generate con
        HExpr.Con      name fields            -> simple  $ convert name <> spaceJoin ("" : sgenmap fields)
        HExpr.CondE    cond sucess failure    -> complex $ "ifThenElse' " <> sgenerate cond <> (simplify.buildDoBlock) sucess <> (simplify.buildDoBlock) failure
        HExpr.RecUpdE  expr name val          -> complex $ sgenerate expr <> " { " <> convert name <> " = " <> generate val <> "}"
        HExpr.Typed    expr cls               -> complex $ generate expr <> " :: " <> generate cls
        HExpr.TypedP   expr cls               -> complex $ generate expr <> " :: " <> generate cls
        HExpr.TypedE   expr cls               -> complex $ generate expr <> " :: " <> generate cls
        HExpr.TySynD   name params dstType    -> complex $ "type " <> convert name <> " " <> spaceJoin (sgenmap params) <> " = " <> generate dstType
        HExpr.Function name signature expr    -> simple  $ convert name <> spaceJoin ("" : genmap signature) <> " = " <> generate expr
        HExpr.Lambda   signature expr         -> simple  $ "(\\" <> spaceJoin (sgenmap signature) <> " -> " <> sgenerate expr <> ")"
        HExpr.LetBlock exprs result           -> simple  $ "let { " <> mjoin "; " (genmap exprs) <> " } in " <> generate result
        HExpr.LetExpr  expr                   -> simple  $ "let " <> generate expr
        HExpr.OperatorE name src dst          -> complex $ sgenerate src  <> " " <> convert name <> " " <> sgenerate dst
        HExpr.Infix     name src dst          -> complex $ sgenerate src  <> " `" <> convert name <> "` " <> sgenerate dst
        HExpr.NOP                             -> simple  $ "nop"
        HExpr.Assignment src dst              -> simple  $ generate src <> " = " <> generate dst
        HExpr.Arrow      src dst              -> simple  $ generate src <> " <- " <> generate dst
        HExpr.TupleP   items                  -> simple  $ "(" <> sepjoin (genmap items) <> ")"
        HExpr.ConE     qname                  -> simple  $ mjoin "." (convert qname)
        HExpr.ConT     name                   -> simple  $ convert name
        HExpr.ConP     name                   -> simple  $ convert name
        HExpr.Native   natCode                -> simple  $ convert natCode
        HExpr.ListE    items                  -> simple  $ "[" <> sepjoin (genmap items) <> "]"
        HExpr.ListT    item                   -> simple  $ "[" <> generate item <> "]"
        HExpr.Lit      val                    -> generate val
        HExpr.LitT     val                    -> generate val
        HExpr.Bang     expr                   -> simple  $ "--->>>   " <> generate expr
        HExpr.THE      expr                   -> simple  $ "$(" <> generate expr <> ")"
        HExpr.CaseE    expr matches           -> complex $ "case " <> generate expr <> " of {" <> buildBody matches <> "}"
        HExpr.Match    pat matchBody          -> complex $ generate pat <> " -> " <> generate matchBody
        HExpr.ViewP    expr dst               -> simple  $ "(" <> generate expr <> " -> " <> generate dst <> ")"
        HExpr.Import   q segments rename      -> simple  $ "import "
                                                         <> if q then "qualified " else ""
                                                         <> mjoin "." (fmap convert segments)
                                                         <> case rename of
                                                                 Just name -> " as " <> (convert name)
                                                                 Nothing   -> ""
        HExpr.DataD    name params cons ders  -> simple  $ "data " <> convert name <> " " <> spaceJoin (fmap convert params) <> " = " <> cons' <> convert ders'
                                                         where cons'   = mjoin " | " (genmap cons)
                                                               ders'   = if null ders then "" else " deriving (" <> sepjoin (map show ders) <> ")"
        HExpr.Comment  comment                -> generate comment
        HExpr.DoBlock  exprs                  -> buildDoBlock exprs
        HExpr.AppT     src dst                -> app (generate src) (generate dst) --"(" <> generate src <> " (" <> generate dst <> ")" <> ")" -- for literals, e.g. simple (1 :: Int)
        HExpr.AppE     src dst                -> app (generate src) (generate dst) --"(" <> generate src <> " " <> generate dst <> ")"
        HExpr.AppP     src dst                -> app (generate src) (generate dst)
        HExpr.MacroE   name items             -> simple $ appPragma (fromString name) where
                                                     appPragma = if length items == 0 then id
                                                                                      else (<> ("(" <> sepjoin (map generate items) <> ")"))
        HExpr.Tuple    items                  -> if length items == 1 then app (simple "OneTuple") (generate $ head items)
                                                                      else simple $ "(" <> sepjoin (map generate items) <> ")"
        HExpr.Pragma   p                      -> generate p
        HExpr.DataKindT e                     -> simple $ "'" <> generate e
        where sepjoin     = mjoin ", "


instance Generator HPragma where
    generate p = simple $ case p of
        HExpr.Include name -> "#include \"" <> fromString name <> "\""

instance Generator HComment where
    generate comment = simple $ case comment of
        HComment.H1 str -> mkSpace 2 <> "-- " <> convert (replicate 67 '=') <> "\n-- " <> convert str <> "\n" <> "-- " <> convert (replicate 67 '=')
        HComment.H2 str -> mkSpace 1 <> "-- ====== " <> convert str <> " ====== --"
        HComment.H3 str -> mkSpace 1 <> "-- ------ " <> convert str <> " ------ --"
        HComment.H4 str -> "-- --- " <> convert str <> " --- --"
        HComment.H5 str -> "-- " <> convert str
        where mkSpace n = convert $ replicate n '\n'


instance Generator HLit.Lit where
    generate lit = simple $ case lit of
        HLit.Integer val -> convert $ escapeNegative (toList val)
        HLit.Int     val -> convert $ escapeNegative (toList val)
        HLit.Float   val -> convert $ escapeNegative (toList val)
        HLit.String  val -> "\"" <> convert val   <> "\""
        HLit.Char    val -> "'"  <> convert [val] <> "'"
        where escapeNegative num@('-':_) = "(" <> num <> ")"
              escapeNegative num         = num
