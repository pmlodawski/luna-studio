---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Luna.DEP.Util.LunaShow where

import qualified Data.List               as List
import qualified Data.Maybe              as Maybe

import           Flowbox.Prelude         hiding (simple)
import           Luna.DEP.AST.Arg        (Arg)
import qualified Luna.DEP.AST.Arg        as Arg
import           Luna.DEP.AST.Expr       (Expr)
import qualified Luna.DEP.AST.Expr       as Expr
import           Luna.DEP.AST.Lit        (Lit)
import qualified Luna.DEP.AST.Lit        as Lit
import           Luna.DEP.AST.Lit.Number (Number (Number))
import qualified Luna.DEP.AST.Lit.Number as Number
import           Luna.DEP.AST.Name       (Name)
import qualified Luna.DEP.AST.Name       as Name
import           Luna.DEP.AST.Pat        (Pat)
import qualified Luna.DEP.AST.Pat        as Pat
import           Luna.DEP.AST.Type       (Type)
import qualified Luna.DEP.AST.Type       as Type


data ShowContext = ShowContext { _accessorContent :: Bool }
                               deriving Show

makeLenses ''ShowContext


instance Default ShowContext where
    def = ShowContext False


class LunaShow ast where
    lunaShow  ::                ast -> String
    lunaShow  = code . lunaShowC def
    lunaShowC :: ShowContext -> ast -> CodeBuilder String


instance LunaShow Expr where
    lunaShowC context expr =  case expr of
        Expr.Accessor     _ acc      dst  -> case Expr.mkAccessor $ acc ^. Expr.accName of
            Expr.ConAccessor accName -> simple  [accName, " ", csLunaShow (context & accessorContent .~ True) dst]
            Expr.VarAccessor accName -> simple  [csLunaShow (context & accessorContent .~ True) dst, ".", accName]
        Expr.App          _ src      args -> app $ csLunaShow context src : if f
                                                then ["(", List.intercalate ", " $ map (csLunaShow $ accessorContent .~ False $ context) args, ")"]
                                                else [unwords $ "" : map (csLunaShow $ accessorContent .~ False $ context) args]
                                             where app = if null args || f then simple else complex
                                                   f = context ^. accessorContent && length args > 1
        --Expr.AppCons_     _ args
        Expr.Assignment   _ pat      dst  -> simple  [csLunaShow context pat, " = ", cLunaShow context dst]
        --Expr.RecordUpdate _ name     selectors expr
        --Expr.Data         _ cls      cons      classes methods
        --Expr.ConD         _ name     fields
        Expr.Con          _ name          -> simple  [name]
        Expr.Function _ path name inputs output body -> simple  ["def "
                                                                , if null path then "" else List.intercalate "." path ++ "."
                                                                , csLunaShow context name
                                                                , [' ' | not $ null inputs]
                                                                , unwords $ map (csLunaShow context) inputs
                                                                , if isUnknown output then "" else " -> " ++ cLunaShow context output
                                                                , if null body then "" else ":\n    " ++ List.intercalate "\n    " (map (cLunaShow context) body)
                                                                , "\n"
                                                                ]
        Expr.Grouped      _ grouped       -> simple  ["(", cLunaShow context grouped, ")"]
        --Expr.Import       _ path     target    rename
        --Expr.Infix        _ name     src       dst
        Expr.List         _ items         -> simple  ["[", List.intercalate ", " $ map (csLunaShow context) items, "]"]
        Expr.Lit          _ lvalue        -> simple  [csLunaShow context lvalue]
        Expr.Tuple        _ items         -> complex [List.intercalate ", " $ map (csLunaShow context) items]
        Expr.Typed        _ cls     expr' -> complex [csLunaShow context expr', " :: ", csLunaShow context cls]
        Expr.Var          _ name          -> simple  [name]
        Expr.Wildcard     _               -> simple  ["_"]
        Expr.RangeFromTo  _ start    end  -> simple  [csLunaShow context start, "..", csLunaShow context end]
        Expr.RangeFrom    _ start         -> simple  [csLunaShow context start, ".."]
        --Expr.Field        _ name     cls       value
        Expr.Arg          _ pat value     -> simple  [csLunaShow context pat, Maybe.maybe "" (\e -> '=':csLunaShow context e) value]
        Expr.Native       _ segments      -> simple  ["```", concatMap (csLunaShow context) segments, "```"]
        Expr.NativeCode   _ code'         -> simple  [code']
        Expr.NativeVar    _ name          -> simple  ["#{", name, "}"]
        --Expr.Case         _ expr     match
        --Expr.Match        _ pat      body
        _ -> error $ "lunaShow: Not implemented: " ++ show expr
        where
            isUnknown (Type.Unknown {}) = True
            isUnknown _                 = False


instance LunaShow (Arg Expr) where
    lunaShowC context arg = case arg of
        --Arg.Named _ name a ->
        Arg.Unnamed _ a -> pure $ csLunaShow context a


instance LunaShow Name where
    lunaShowC _ name = pure $ name ^. Name.base


instance LunaShow Lit where
    lunaShowC context lit = pure $ case lit of
        Lit.Char    _ char -> '\'' : char : "'"
        Lit.String  _ str  -> '\"' : str ++ "\""
        Lit.Number  _ num  -> csLunaShow context num


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


instance LunaShow Pat where
    lunaShowC context p = simple  $ case p of
        Pat.Var      _ name      -> [name]
        Pat.Lit      _ value     -> [csLunaShow context value]
        Pat.Tuple    _ items     -> [List.intercalate ", " $ map (csLunaShow context) items]
        Pat.Con      _ name      -> [name]
        Pat.App      _ src args  -> [csLunaShow context src, " ", unwords $ map (csLunaShow context) args]
        Pat.Typed    _ pat cls   -> [csLunaShow context pat, " :: ", csLunaShow context cls]
        Pat.Grouped  _ pat       -> ["(", csLunaShow context pat, ")"]
        Pat.Wildcard _           -> ["_"]
        Pat.RecWildcard _        -> [".."]


instance LunaShow Type where
    lunaShowC context t = case t of
        Type.Unknown _           -> simple  ["Unknown"]
        Type.Var     _ name      -> simple  [name]
        Type.Tuple   _ items     -> simple  ["(", List.intercalate ", " $ map (csLunaShow context) items, ")"]
        Type.List    _ item      -> simple  ["[", csLunaShow context item, "]"]
        Type.App     _ src args  -> complex [unwords $ map (csLunaShow context) $ src : args]
        --Type.Class   _ name params' -> name ++ " " ++ (List.intercalate " " params')
        --Type.Module  _ path'         -> List.intercalate "." path'
        Type.Con     _ segments  -> simple  [List.intercalate "." segments]
        _ -> error $ "lunaShow: Not implemented: " ++ show t


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
