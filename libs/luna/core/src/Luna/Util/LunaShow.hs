---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Luna.Util.LunaShow where

import qualified Data.List  as List
import qualified Data.Maybe as Maybe

import           Flowbox.Prelude
import           Luna.AST.Arg        (Arg)
import qualified Luna.AST.Arg        as Arg
import           Luna.AST.Expr       (Expr)
import qualified Luna.AST.Expr       as Expr
import           Luna.AST.Lit        (Lit)
import qualified Luna.AST.Lit        as Lit
import           Luna.AST.Lit.Number (Number (Number))
import qualified Luna.AST.Lit.Number as Number
import           Luna.AST.Name       (Name)
import qualified Luna.AST.Name       as Name
import           Luna.AST.Pat        (Pat)
import qualified Luna.AST.Pat        as Pat
import           Luna.AST.Type       (Type)
import qualified Luna.AST.Type       as Type




data ShowContext = ShowContext { _accessorContent :: Bool }
                               deriving Show

makeLenses ''ShowContext


instance Default ShowContext where
    def = ShowContext False


class LunaShow ast where
    lunaShow  ::                ast -> String
    lunaShow  = lunaShowC def
    lunaShowC :: ShowContext -> ast -> String



instance LunaShow Expr where
    lunaShowC context expr = concat $ case expr of
        Expr.Accessor     _ acc      dst  -> [lunaShowC (context & accessorContent .~ True) dst, ".", view Expr.accName acc]
        Expr.App          _ src      args -> lunaShowC context src : if context ^. accessorContent && length args > 1
                                                then ["(", List.intercalate ", " $ map (lunaShowC $ accessorContent .~ False $ context) args, ")"]
                                                else [unwords $ "" : (map (lunaShowC $ accessorContent .~ False $ context) $ args)]
        --Expr.AppCons_     _ args
        Expr.Assignment   _ pat      dst  -> [lunaShowC context pat, " = ", lunaShowC context dst]
        --Expr.RecordUpdate _ name     selectors expr
        --Expr.Data         _ cls      cons      classes methods
        --Expr.ConD         _ name     fields
        Expr.Con          _ name          -> [name]
        Expr.Function _ path name inputs output body -> ["def "
                                                        , if null path then "" else List.intercalate "." path ++ "."
                                                        , lunaShowC context name
                                                        , [' ' | not $ null inputs]
                                                        , unwords $ map (lunaShowC context) inputs
                                                        , if isUnknown output then "" else " -> " ++ lunaShowC context output
                                                        , if null body then "" else ":\n    " ++ List.intercalate "\n    " (map (lunaShowC context) body)
                                                        , "\n"
                                                        ]
        Expr.Grouped      _ grouped       -> ["(", lunaShowC context grouped, ")"]
        --Expr.Import       _ path     target    rename
        --Expr.Infix        _ name     src       dst
        Expr.List         _ items         -> ["[", List.intercalate ", " $ map (lunaShowC context) items, "]"]
        Expr.Lit          _ lvalue        -> [lunaShowC context lvalue]
        Expr.Tuple        _ items         -> [List.intercalate ", " $ map (lunaShowC context) items]
        --Expr.Typed        _ cls      expr
        Expr.Var          _ name          -> [name]
        Expr.Wildcard     _               -> ["_"]
        Expr.RangeFromTo  _ start    end  -> [lunaShowC context start, "..", lunaShowC context end]
        Expr.RangeFrom    _ start         -> [lunaShowC context start, ".."]
        --Expr.Field        _ name     cls       value
        Expr.Arg          _ pat value     -> [lunaShowC context pat, Maybe.maybe "" (\e -> '=':lunaShowC context e) value]
        Expr.Native       _ segments      -> ["```", concatMap (lunaShowC context) segments, "```"]
        Expr.NativeCode   _ code          -> [code]
        Expr.NativeVar    _ name          -> ["#{", name, "}"]
        --Expr.Case         _ expr     match
        --Expr.Match        _ pat      body
        _ -> error $ "lunaShow: Not implemented: " ++ show expr
        where
            isUnknown (Type.Unknown {}) = True
            isUnknown _                 = False


instance LunaShow (Arg Expr) where
    lunaShowC context arg = case arg of
        --Arg.Named _ name a ->
        Arg.Unnamed _ a -> lunaShowC context a


instance LunaShow Name where
    lunaShowC context name = name ^. Name.base


instance LunaShow Lit where
    lunaShowC context lit = case lit of
        Lit.Char    _ char -> '\'' : char : "'"
        Lit.String  _ str  -> '\"' : str ++ "\""
        Lit.Number  _ num  -> lunaShowC context num


instance LunaShow Number where
    lunaShowC context (Number base' repr' exp' sign') = concat [showSign sign', showRepr repr', showExp base' exp'] where
        showSign Number.Positive = ""
        showSign Number.Negative = "-"
        showRepr (Number.Float int' frac') = concat [int', ".", frac']
        showRepr (Number.Decimal int')     = int'
        showExp _ Nothing = ""
        -- FIXME [PM] : Implement other bases than 10 and 16!
        showExp 10 (Just num) = "E" ++ lunaShowC context num
        showExp 16 (Just num) = "P" ++ lunaShowC context num


instance LunaShow Pat where
    lunaShowC context p = concat $ case p of
        Pat.Var      _ name      -> [name]
        Pat.Lit      _ value     -> [lunaShowC context value]
        Pat.Tuple    _ items     -> [List.intercalate ", " $ map (lunaShowC context) items]
        Pat.Con      _ name      -> [name]
        Pat.App      _ src args  -> [lunaShowC context src, " ", unwords $ map (lunaShowC context) args]
        Pat.Typed    _ pat cls   -> [lunaShowC context pat, " :: ", lunaShowC context cls]
        Pat.Grouped  _ pat       -> ["(", lunaShowC context pat, ")"]
        Pat.Wildcard _           -> ["_"]
        Pat.RecWildcard _        -> [".."]


instance LunaShow Type where
    lunaShowC context t = concat $ case t of
        Type.Unknown _           -> ["Unknown"]
        Type.Var     _ name      -> [name]
        Type.Tuple   _ items     -> ["(", List.intercalate ", " $ map (lunaShowC context) items, ")"]
        Type.List    _ item      -> ["[", lunaShowC context item, "]"]
        --Type.Class   _ name params' -> name ++ " " ++ (List.intercalate " " params')
        --Type.Module  _ path'         -> List.intercalate "." path'
        Type.Con     _ segments  -> [List.intercalate "." segments]
        _ -> error $ "lunaShow: Not implemented: " ++ show t
