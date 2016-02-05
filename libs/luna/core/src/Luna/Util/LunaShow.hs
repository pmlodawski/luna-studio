---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Luna.Util.LunaShow where

import qualified Data.List                as List
import qualified Data.Maybe               as Maybe


import           Flowbox.Prelude          hiding (simple)
import           Luna.Syntax.Expr         (LExpr)
import qualified Luna.Syntax.Expr         as Expr
import           Luna.Syntax.Lit          (LLit)
import           Luna.Syntax.Lit          (LLit)
import qualified Luna.Syntax.Lit          as Lit
import           Luna.Syntax.Lit.Number   (Number (Number))
import qualified Luna.Syntax.Lit.Number   as Number
import qualified Luna.Syntax.Name         as Name
import qualified Luna.Syntax.Name.Pattern as Pattern
import           Luna.Syntax.Pat          (LPat)
import qualified Luna.Syntax.Pat          as Pat
import           Luna.Syntax.Type         (LType)
import qualified Luna.Syntax.Type         as Type
--import           Luna.AST.Arg        (Arg)
--import qualified Luna.AST.Arg        as Arg
--import           Luna.DEP.AST.Name       (Name)
--import qualified Luna.DEP.AST.Name       as Name



data ShowContext = ShowContext { _accessorContent :: Bool } deriving Show

makeLenses ''ShowContext


instance Default ShowContext where
    def = ShowContext False


class LunaShow ast where
    lunaShow  ::                ast -> String
    lunaShow  = code . lunaShowC def
    lunaShowC :: ShowContext -> ast -> CodeBuilder String


instance (Show a, Show v) => LunaShow (LExpr a v) where
    lunaShowC context lexpr =  case unwrap lexpr of
        --Expr.Lambda     inputs output body ->
        --Expr.RecUpd     vname  fieldUpds   ->
        --Expr.Case       expr   match       ->
        Expr.Typed      cls    expr        -> complex [csLunaShow context expr, " :: ", csLunaShow context expr]
        Expr.Assignment dst    src         -> simple  [csLunaShow context dst, " = ", cLunaShow context src ]
        Expr.Accessor   acc    src         -> simple  [csLunaShow (context & accessorContent .~ True) src, ".", csLunaShow context acc]
        --Expr.Curry      expr               ->
        --Expr.Meta       meta               ->
        Expr.Tuple      items              -> complex [List.intercalate ", " $ map (csLunaShow context) items]
        Expr.Grouped    expr               -> simple  ["(", cLunaShow context expr, ")"]
        Expr.Cons       cname              -> simple  [toString cname]
        --Expr.Decl       decl               -> simple  [csLunaShow context decl]
        Expr.Lit        lit                -> simple  [csLunaShow context lit]
        --Expr.Native     native             -> simple  [csLunaShow context native]
        Expr.Var        ident              -> simple  [csLunaShow context ident]
        Expr.List       list               -> simple  [csLunaShow context list]
        Expr.App        exprApp            -> simple  [cLunaShow context exprApp]
        Expr.Wildcard                      -> simple  ["_"]
        _ -> error $ "LunaShow (LExpr a v): Not implemented" ++ show (unwrap lexpr)
--        Expr.Accessor     _ acc      dst  -> simple  [csLunaShow (context & accessorContent .~ True) dst, ".", view Expr.accName acc]
--        Expr.App          _ src      args -> app $ csLunaShow context src : if f
--                                                then ["(", List.intercalate ", " $ map (csLunaShow $ accessorContent .~ False $ context) args, ")"]
--                                                else [unwords $ "" : (map (csLunaShow $ accessorContent .~ False $ context) $ args)]
--                                             where app = if null args || f then simple else complex
--                                                   f = context ^. accessorContent && length args > 1
--        Expr.Assignment   _ pat      dst  -> simple  [csLunaShow context pat, " = ", cLunaShow context dst]
--        Expr.Function _ path name inputs output body -> simple  ["def "
--                                                                , if null path then "" else List.intercalate "." path ++ "."
--                                                                , csLunaShow context name
--                                                                , [' ' | not $ null inputs]
--                                                                , unwords $ map (csLunaShow context) inputs
--                                                                , if isUnknown output then "" else " -> " ++ csLunaShow context output
--                                                                , if null body then "" else ":\n    " ++ List.intercalate "\n    " (map (cLunaShow context) body)
--                                                                , "\n"
--                                                                ]
--        Expr.RangeFromTo  _ start    end  -> simple  [csLunaShow context start, "..", csLunaShow context end]
--        Expr.RangeFrom    _ start         -> simple  [csLunaShow context start, ".."]
--        Expr.Arg          _ pat value     -> simple  [csLunaShow context pat, Maybe.maybe "" (\e -> '=':csLunaShow context e) value]
--        Expr.Native       _ segments      -> simple  ["```", concatMap (csLunaShow context) segments, "```"]
--        Expr.NativeCode   _ code          -> simple  [code]
--        Expr.NativeVar    _ name          -> simple  ["#{", name, "}"]
--        _ -> error $ "lunaShow: Not implemented: " ++ show expr
--        where
--            isUnknown (Type.Unknown {}) = True
--            isUnknown _                 = False


--instance LunaShow (Arg Expr) where
--    lunaShowC context arg = case arg of
--        --Arg.Named _ name a ->
--        Arg.Unnamed _ a -> pure $ csLunaShow context a


--instance LunaShow Name where
--    lunaShowC context name = pure $ name ^. Name.base

instance LunaShow Name.NameBaseP where
    lunaShowC _ = pure . toString . unwrap


instance (LunaShow base, LunaShow arg) => LunaShow (Pattern.NamePat base arg) where
    lunaShowC context (Pattern.NamePat prefix base segmentList) = complex [unwords $ Maybe.maybeToList (fmap (csLunaShow context) prefix)
                                                                                  ++ [csLunaShow context base]
                                                                                  ++ map (csLunaShow context) segmentList]

instance (LunaShow base, LunaShow arg) => LunaShow (Pattern.Segment base arg) where
    lunaShowC context (Pattern.Segment base args) = simple [unwords $ csLunaShow context base : map (csLunaShow context) args]

instance LunaShow e => LunaShow (Expr.AppArg e) where
    lunaShowC context (Expr.AppArg Nothing e) = simple [csLunaShow context e]

instance LunaShow Text where
    lunaShowC _ = pure . toString

instance LunaShow (Expr.Variable v) where
    lunaShowC context (Expr.Variable vname _) = pure $ toString vname

instance LunaShow e => LunaShow (Expr.List e) where
    lunaShowC context (Expr.SeqList items) = simple  ["[", List.intercalate ", " $ map (csLunaShow context) items, "]"]

instance LunaShow (LLit a) where
    lunaShowC context llit = pure $ case unwrap llit of
        Lit.Char   char -> '\'' : char : "'"
        Lit.String str  -> '\"' : str ++ "\""
        Lit.Number num  -> csLunaShow context num


instance LunaShow Number where
    lunaShowC context (Number base' repr' exp' sign') = simple  [showSign sign', showRepr repr', showExp base' exp'] where
        showSign Number.Positive = ""
        showSign Number.Negative = "-"
        showRepr (Number.Float int' frac') = concat [int', ".", frac']
        showRepr (Number.Decimal int')     = int'
        showExp _ Nothing = ""
        -- FIXME [PM] : Implement other bases than 10 and 16!
        showExp 10 (Just num) = "E" ++ csLunaShow context num
        showExp 16 (Just num) = "P" ++ csLunaShow context num


instance Show a => LunaShow (LPat a) where
    lunaShowC context lpat = case unwrap lpat of
        Pat.App     src args -> simple [csLunaShow context src, " ", unwords $ map (csLunaShow context) args]
        Pat.Typed   pat cls  -> simple [csLunaShow context pat, " :: ", csLunaShow context cls]
        Pat.Grouped pat      -> simple ["(", cLunaShow context pat, ")"]
        Pat.Lit     lit      -> simple [csLunaShow context lit]
        Pat.Tuple   items    -> simple [List.intercalate ", " $ map (csLunaShow context) items]
        Pat.Con     cname    -> simple [toString cname]
        Pat.Var     vname    -> simple [toString vname]
        Pat.Wildcard         -> simple ["_"]
        Pat.RecWildcard      -> simple [".."]

instance Show a => LunaShow (LType a) where
    lunaShowC context ltype = case unwrap ltype of
        Type.Var      vname         -> simple [toString vname]
        Type.Tuple    items         -> simple ["(", List.intercalate ", " $ map (csLunaShow context) items, ")"]
        Type.List     item          -> simple ["[", csLunaShow context item, "]"]
        Type.Con      segments      -> simple [List.intercalate "." $ map toString segments]
        Type.Wildcard               -> simple ["_"]
        _ -> error $ "LunaShow (LType a): Not implemented" ++ show (unwrap ltype)


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

simplify :: CodeBuilder String -> CodeBuilder String
simplify c = case c of
    Simple {} -> c
    Complex v -> Simple $ "(" ++ v ++ ")"


cLunaShow :: LunaShow a => ShowContext -> a -> String
cLunaShow = code .: lunaShowC

csLunaShow :: LunaShow a => ShowContext -> a -> String
csLunaShow = (code . simplify) .: lunaShowC

simple :: [String] -> CodeBuilder String
simple = pure . concat

complex :: [String] -> CodeBuilder String
complex = Complex . concat
