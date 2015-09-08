---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Pass.Target.HS.HSC where

import           Control.Monad.State           hiding (mapM)
import           Data.List                     (intersperse)
import           Data.Text.CodeBuilder.Builder
import           Data.Text.CodeBuilder.Doc     (between, line, nested, (</>))
import qualified Data.Text.CodeBuilder.Doc     as Doc
import           Data.Text.CodeBuilder.Tok     (Prec (Top), Tok (Tok))
import qualified Data.Text.CodeBuilder.Tok     as Tok
import           Data.Text.Lazy                (Text)
import qualified Data.Text.Lazy                as Text
import           Data.Text.Lazy.Builder        (fromLazyText, toLazyText)
import qualified Data.Text.Lazy.Builder        as Text

import           Flowbox.Prelude              hiding (assign, cons, simple)
import           Luna.Data.Source             (Source (Source))
import           Luna.Pass                    (Pass (Pass), PassMonad)
import           Luna.Pass.Pass               (Pass)
import qualified Luna.Pass.Pass               as Pass
import qualified Luna.Target.HS.AST.Comment   as HComment
import qualified Luna.Target.HS.AST.Expr      as HExpr
import           Luna.Target.HS.AST.Extension (Extension)
import qualified Luna.Target.HS.AST.Lit       as HLit

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
genModule (HExpr.Module name path ext imports body) = toLazyText modcode where
    modcode =  genSection    "extensions"     genExt  ext
            <> sectionHeader "module"         <> header
            <> genSection    "imports"        generate2' imports
            <> genSection    "body"           generate2' body
    header = "module " <> mjoin "." (fmap fromLazyText (path <> [name])) <> " where" <> eol <> eol

genExt :: Extension -> Text.Builder
genExt ext = "{-# LANGUAGE " <> show' ext <> " #-}"

genSection :: Text.Builder -> (a -> Text.Builder) -> [a] -> Text.Builder
genSection header generator d = if null d
    then ""
    else sectionHeader header <> mjoin "\n"  (map generator d) <> "\n\n"


------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------


thed = fmap $ Tok Top . between "$(" ")" .view Tok.doc

runMeI = renderCode HSIndent
runMeC = renderCode HSCompact

genExpr :: HExpr -> Text
genExpr = toLazyText . generate2'






eol :: Text.Builder
eol = "\n"

sectionHeader :: Text.Builder -> Text.Builder
sectionHeader name = "-- " <> name <> " --\n"


spaceJoin :: (Monoid a, IsString a) => [a] -> a
spaceJoin = mjoin " "

--buildDoBlock :: CodeBuilder a => [HExpr] -> a
--buildDoBlock exprs = complex $ "do { " <> buildBody exprs <> " }"

--buildBody :: (IsString a, CodeBuilder a, Monoid a) => [HExpr] -> a
--buildBody exprs = if null exprs then "" else mjoin "; " (genmap exprs) <> ";"

generate2' a = runMeI $ generate2 a

class Generator2 a s where
    generate2 :: a -> Builder s Tok

class AutoGen a b where
    gen :: a -> b

instance (AutoGen a b, out~[b]) => AutoGen [a] out where
    gen = fmap gen

instance (Generator2 a s, out~Builder s Tok) => AutoGen a out where
    gen = generate2


--instance Convertible Text CB.Code where
--    safeConvert = Right . tok

instance Convertible Text (Builder s Tok) where
    safeConvert = Right . pure . fromText

instance (s~s') => Convertible (Builder s Tok) (Builder s' Tok) where
    safeConvert = Right

instance Convertible String (Builder s Tok) where
    safeConvert = Right . pure . fromString

--dataDecl name params cons = app (apps "data" $ name : params) cons
dataDecl name params cons = app' (apps' ("data"::String) $ name : params) cons

func name args body = apps' name args `assign` sbox body

app' a b = app (convert a) (convert b)
apps' base = foldl app' (convert base)

macroApps name items = sbox $ app name $ tuple items where
    app a b = appWith "" L 10 (convert a) (convert b)

buildBody2 exprs = if null exprs then "" else mjoin "; " (gen exprs) <> ";"


tuple' = tuple . fmap convert

doBlock = \case
    []    -> error "empty do-block!"
    [x]   -> x
    items -> app "do" . block $ items

lineBlock = \case
    []    -> error "empty line-block!"
    [x]   -> x
    items -> block items

typed base t = apps' base ["::", t]

assign a b = apps' a ["=", b]
arrow a b = apps' a ["->", b]

removeEmptyBegining = \case
    [] -> []
    (x:xs) -> case x of
        "" -> removeEmptyBegining xs
        _  -> x:xs

instance (Render s ConsBlock, Render s Block) => Generator2 HExpr s where
    generate2 = \case
        HExpr.WildP                           -> "_"
        HExpr.RecWildP                        -> "{}"
        HExpr.NOP                             -> "nop"
        HExpr.Var      name                   -> convert name
        HExpr.VarE     name                   -> convert name
        HExpr.VarT     name                   -> convert name
        HExpr.ConT     name                   -> convert name
        HExpr.ConP     name                   -> convert name
        HExpr.Native   natCode                -> lineBlock lines where
                                                 lines = fmap fromString
                                                       $ removeEmptyBegining
                                                       $ fmap (fromText . Text.strip)
                                                       $ Text.lines natCode
        HExpr.Pragma   p                      -> gen p
        HExpr.Lit      val                    -> gen val
        HExpr.LitT     val                    -> gen val
        HExpr.Comment  comment                -> gen comment
        HExpr.DataKindT e                     -> "'" <> gen e
        HExpr.ImportNative code               -> app' ("import "::String) code
        HExpr.AppP     src dst                -> app' (gen src) (gen dst)
        HExpr.AppT     src dst                -> app' (gen src) (gen dst)
        HExpr.AppE     src dst                -> app' (gen src) $ gen dst
        HExpr.RecUpdE  expr name val          -> app' (gen expr) $ braced $ name `assign` gen val
        HExpr.InstanceD tp decs               -> "instance " <> gen tp <> " where { " <> mjoin "; " (gen decs) <> " }"
        HExpr.TypeInstance tp expr            -> app' ("type instance" :: String) (gen tp) `assign` gen expr
        HExpr.NewTypeD name params con  ders  -> apps' ("newtype" :: String) (name : params) `app'` consBlock [gen con] (fmap (fromString . show) ders)
        HExpr.DataD    name params cons ders  -> dataDecl name params $ consBlock (gen cons) (fmap (fromString . show) ders)
        HExpr.TypeD    dst src                -> app' ("type" :: String) (gen dst) `assign` gen src
        HExpr.Con      name fields            -> apps' name $ gen fields
        HExpr.THE      expr                   -> thed $ gen expr
        HExpr.Function name signature expr    -> func name (gen signature) $ gen expr
        HExpr.MacroE   name items             -> macroApps name $ gen items
        HExpr.Tuple    items                  -> tuple' $ gen items
        HExpr.DoBlock  exprs                  -> doBlock $ gen exprs
        HExpr.TypedE   expr cls               -> typed (gen expr) $ gen cls
        HExpr.Typed    expr cls               -> typed (gen expr) $ gen cls
        HExpr.TypedP   expr cls               -> typed (gen expr) $ gen cls
        HExpr.TySynD   name params dstType    -> "type " <> convert name <> " " <> spaceJoin (gen params) <> " = " <> gen dstType
        HExpr.Lambda   signature expr         -> "(\\" <> spaceJoin (gen signature) <> " -> " <> gen expr <> ")"
        HExpr.LetBlock exprs result           -> "let { " <> mjoin "; " (gen exprs) <> " } in " <> gen result
        HExpr.LetExpr  expr                   -> "let " <> gen expr
        HExpr.OperatorE name src dst          -> gen src `app'` name `app'` gen dst
        HExpr.Infix     name src dst          -> gen src  <> " `" <> convert name <> "` " <> gen dst
        HExpr.Assignment src dst              -> gen src `assign` gen dst
        HExpr.Arrow      src dst              -> gen src <> " <- " <> gen dst
        HExpr.TupleP   items                  -> tuple' $ gen items
        HExpr.ConE     qname                  -> mjoin "." (convert qname)
        HExpr.ListE    items                  -> list $ gen items
        HExpr.ListT    item                   -> list [gen item]
        HExpr.Bang     expr                   -> "--->>>   " <> gen expr
        HExpr.Match    pat matchBody          -> gen pat `arrow` gen matchBody
        HExpr.ViewP    expr dst               -> parensed $ gen expr `arrow` gen dst
        HExpr.Import   q segments rename tgts -> "import "
                                                         <> (if q then "qualified " else "")
                                                         <> mjoin "." (fmap convert segments)
                                                         <> maybe "" (\name -> " as " <> convert name) rename
                                                         <> maybe "" (\lst  -> " (" <> sepjoin (fmap convert lst) <> ")") tgts
        --HExpr.CaseE    expr matches           -> "case " <> gen expr <> " of {" <> buildBody2 matches <> "}"
        HExpr.CaseE    expr matches           -> apps "case" [gen expr, "of", block $ gen matches] -- " of {" <> buildBody2 matches <> "}"
        HExpr.LambdaCase matches              -> app "\\case" (block $ gen matches)
        --HExpr.CondE    cond sucess failure    -> complex $ "ifThenElse' " <> gen cond <> (simplify.buildDoBlock) sucess <> (simplify.buildDoBlock) failure

        s -> tok 10 $ fromString $ "unknown: " ++ show s
        where sepjoin     = mjoin ", "

instance Generator2 HPragma s where
    generate2 = \case
        HExpr.Include name -> app "#include" (fromString $ "\"" <> name <> "\"")

instance Generator2 HComment s where
    generate2 c = tok Tok.Top . fromText $ case c of
        HComment.H1 str -> mkSpace 2 <> "-- " <> convert (replicate 67 '=') <> "\n-- " <> str <> "\n" <> "-- " <> convert (replicate 67 '=')
        HComment.H2 str -> mkSpace 1 <> "-- ====== " <> str <> " ====== --"
        HComment.H3 str -> mkSpace 1 <> "-- ------ " <> str <> " ------ --"
        HComment.H4 str -> "-- --- " <> str <> " --- --"
        HComment.H5 str -> "-- " <> str
        where mkSpace n = convert $ replicate n '\n'

instance Generator2 HLit.Lit m where
    generate2 lit = tok Tok.Top . fromText $ case lit of
        HLit.Integer val -> fromString $ escapeNegative (toList val)
        HLit.Int     val -> fromString $ escapeNegative (toList val)
        HLit.Float   val -> fromString $ escapeNegative (toList val)
        HLit.String  val -> between "\"" "\"" (fromText val)
        HLit.Char    val -> between "'" "'" $ fromString [val]
        where escapeNegative num@('-':_) = "(" <> num <> ")"
              escapeNegative num         = num



------------------------------------------------------------------------
-- Render styles
------------------------------------------------------------------------

-- === Syntax constructors ===

data Block = Block [Tok]
block :: Render s Block => [Builder s Tok] -> Builder s Tok
block items = renderStyled . Block =<< sequence items

data ConsBlock = ConsBlock [Text] [Tok]
consBlock items ders = renderStyled . ConsBlock ders =<< sequence items


-- === Styles ===

data HSIndent  = HSIndent  deriving (Show)
data HSCompact = HSCompact deriving (Show)


instance Render HSIndent Block where
    render _ (Block items) = Tok Top . nested
                                     . foldl (</>) mempty
                                     $ fmap (view Tok.doc) items

instance Render HSCompact Block where
    render _ (Block items) = Tok Top . nested
                                     . between "{" "}"
                                     . foldl (<>) mempty
                                     $ fmap ((<>";") . view Tok.doc) items


instance Render HSIndent ConsBlock where
    render _ (ConsBlock ders items) = Tok Top $ nested (genDeriving ders cons)
        where cons = foldl concat mempty
                   . zip ("= " : repeat "| ")
                   $ fmap (view Tok.doc) items
              concat a (sep,b) = a <> line <> sep <> b


genDeriving ders = case ders of
    [] -> id
    _  -> (<> (line <> "deriving "
                    <> Doc.parensed (mjoin "," $ fmap fromText ders)))
