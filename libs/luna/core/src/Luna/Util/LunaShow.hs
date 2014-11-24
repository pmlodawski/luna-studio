---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

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



class LunaShow ast where
    lunaShow :: ast -> String


instance LunaShow Expr where
    lunaShow expr = concat $ case expr of
        Expr.Accessor     _ acc      dst  -> [lunaShow dst, ".", view Expr.accName acc]
        Expr.App          _ src      args -> [unwords $ lunaShow src : map lunaShow args]
        --Expr.AppCons_     _ args
        Expr.Assignment   _ pat      dst  -> [lunaShow pat, " = ", lunaShow dst]
        --Expr.RecordUpdate _ name     selectors expr
        --Expr.Data         _ cls      cons      classes methods
        --Expr.ConD         _ name     fields
        Expr.Con          _ name          -> [name]
        Expr.Function _ path name inputs output body -> ["def "
                                                        , if null path then "" else List.intercalate "." path ++ "."
                                                        , lunaShow name
                                                        , [' ' | not $ null inputs]
                                                        , unwords $ map lunaShow inputs
                                                        , if isUnknown output then "" else " -> " ++ lunaShow output
                                                        , if null body then "" else ":\n    " ++ List.intercalate "\n    " (map lunaShow body)
                                                        , "\n"
                                                        ]
        Expr.Grouped      _ grouped       -> ["(", lunaShow grouped, ")"]
        --Expr.Import       _ path     target    rename
        --Expr.Infix        _ name     src       dst
        Expr.List         _ items         -> ["[", List.intercalate ", " $ map lunaShow items, "]"]
        Expr.Lit          _ lvalue        -> [lunaShow lvalue]
        Expr.Tuple        _ items         -> [List.intercalate ", " $ map lunaShow items]
        --Expr.Typed        _ cls      expr
        Expr.Var          _ name          -> [name]
        Expr.Wildcard     _               -> ["_"]
        Expr.RangeFromTo  _ start    end  -> [lunaShow start, "..", lunaShow end]
        Expr.RangeFrom    _ start         -> [lunaShow start, ".."]
        --Expr.Field        _ name     cls       value
        Expr.Arg          _ pat value     -> [lunaShow pat, Maybe.maybe "" (\e -> '=':lunaShow e) value]
        Expr.Native       _ segments      -> ["```", concatMap lunaShow segments, "```"]
        Expr.NativeCode   _ code          -> [code]
        Expr.NativeVar    _ name          -> ["#{", name, "}"]
        --Expr.Case         _ expr     match
        --Expr.Match        _ pat      body
        _ -> error $ "lunaShow: Not implemented: " ++ show expr
        where
            isUnknown (Type.Unknown {}) = True
            isUnknown _                 = False


instance LunaShow (Arg Expr) where
    lunaShow arg = case arg of
        --Arg.Named _ name a ->
        Arg.Unnamed _ a -> lunaShow a


instance LunaShow Name where
    lunaShow name = name ^. Name.base


instance LunaShow Lit where
    lunaShow lit = case lit of
        Lit.Char    _ char -> '\'' : char : "'"
        Lit.String  _ str  -> '\"' : str ++ "\""
        Lit.Number  _ num  -> lunaShow num


instance LunaShow Number where
    lunaShow (Number base' repr' exp' sign') = concat [showSign sign', showRepr repr', showExp base' exp'] where
        showSign Number.Positive = ""
        showSign Number.Negative = "-"
        showRepr (Number.Float int' frac') = concat [int', ".", frac']
        showRepr (Number.Decimal int')     = int'
        showExp _ Nothing = ""
        -- FIXME [PM] : Implement other bases than 10 and 16!
        showExp 10 (Just num) = "E" ++ lunaShow num
        showExp 16 (Just num) = "P" ++ lunaShow num


instance LunaShow Pat where
    lunaShow p = concat $ case p of
        Pat.Var      _ name      -> [name]
        Pat.Lit      _ value     -> [lunaShow value]
        Pat.Tuple    _ items     -> [List.intercalate ", " $ map lunaShow items]
        Pat.Con      _ name      -> [name]
        Pat.App      _ src args  -> [lunaShow src, " ", unwords $ map lunaShow args]
        Pat.Typed    _ pat cls   -> [lunaShow pat, " :: ", lunaShow cls]
        Pat.Grouped  _ pat       -> ["(", lunaShow pat, ")"]
        Pat.Wildcard _           -> ["_"]
        Pat.RecWildcard _        -> [".."]


instance LunaShow Type where
    lunaShow t = concat $ case t of
        Type.Unknown _           -> ["Unknown"]
        Type.Var     _ name      -> [name]
        Type.Tuple   _ items     -> ["(", List.intercalate ", " $ map lunaShow items, ")"]
        Type.List    _ item      -> ["[", lunaShow item, "]"]
        --Type.Class   _ name params' -> name ++ " " ++ (List.intercalate " " params')
        --Type.Module  _ path'         -> List.intercalate "." path'
        Type.Con     _ segments  -> [List.intercalate "." segments]
        _ -> error $ "lunaShow: Not implemented: " ++ show t
