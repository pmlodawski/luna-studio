---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Util.LunaShow where

import qualified Data.List as List
import qualified Data.Maybe as Maybe

import           Flowbox.Prelude
import           Luna.AST.Expr   (Expr)
import qualified Luna.AST.Expr   as Expr
import           Luna.AST.Lit    (Lit)
import qualified Luna.AST.Lit    as Lit
import           Luna.AST.Pat    (Pat)
import qualified Luna.AST.Pat    as Pat
import           Luna.AST.Type   (Type)
import qualified Luna.AST.Type   as Type



class LunaShow ast where
    lunaShow :: ast -> String


instance LunaShow Expr where
    lunaShow expr = concat $ case expr of
        Expr.Accessor     _ name     dst  -> [lunaShow dst, ".", name]
        Expr.App          _ src      args -> [unwords $ map lunaShow $ src:args]
        --Expr.AppCons_     _ args
        Expr.Assignment   _ pat      dst  -> [lunaShow pat, " = ", lunaShow dst]
        --Expr.RecordUpdate _ name     selectors expr
        --Expr.Data         _ cls      cons      classes methods
        --Expr.ConD         _ name     fields
        Expr.Con          _ name          -> [name]
        Expr.Function _ path name inputs output body -> ["def "
                                                        , if null path then "" else List.intercalate "." path ++ "." 
                                                        , name
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
        Expr.Tuple        _ items         -> ["{", List.intercalate ", " $ map lunaShow items, "}"]
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



instance LunaShow Lit where
    lunaShow lit = case lit of
        Lit.Char    _ char -> '\'' : char : "'"
        Lit.String  _ str  -> '\"' : str ++ "\""
        Lit.Integer _ str  -> str
        Lit.Float   _ str  -> str


instance LunaShow Pat where
    lunaShow p = case p of
        Pat.Var      _ name      -> name
        Pat.Lit      _ value     -> lunaShow value
        Pat.Tuple    _ items     -> "{" ++ List.intercalate ", " strs ++ "}" where
                                       strs = map lunaShow items
        Pat.Con      _ name      -> name
        Pat.App      _ src args  -> srcStr ++ " " ++ unwords argStrs where
                                       argStrs = map lunaShow args
                                       srcStr  = lunaShow src
        Pat.Typed    _ pat cls   -> patStr ++ " :: " ++ typeStr where
                                       patStr = lunaShow pat
                                       typeStr = lunaShow cls
        Pat.Wildcard _           -> "_"
        Pat.RecWildcard _        -> ".."


instance LunaShow Type where
    lunaShow t = case t of
        Type.Unknown _           -> "Unknown"
        Type.Var     _ name      -> name
        Type.Tuple   _ items     -> "{" ++ List.intercalate ", " strs ++ "}" where
                                       strs = map lunaShow items
        --Type.Class   _ name params' -> name ++ " " ++ (List.intercalate " " params')
        --Type.Module  _ path'         -> List.intercalate "." path'
        Type.Con     _ segments' -> List.intercalate "." segments'
        _ -> error $ "lunaShow: Not implemented: " ++ show t
