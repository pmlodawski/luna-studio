---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Graph.Node.StringExpr where

import Flowbox.Prelude



data StringExpr = Expr    { _string :: String }
                | Tuple
                | Get     { _string :: String }
                | List
                | Id
                | Grouped
                | Pattern { _string :: String }
                | Native  { _string :: String }
                deriving (Show, Eq, Read)

makeLenses ''StringExpr


toString :: StringExpr -> String
toString exprStr = case exprStr of
    Expr    str -> str
    Tuple       -> "Tuple"
    Get     str -> "get " ++ str
    List        -> "List"
    Id          -> "id"
    Grouped     -> "Grouped"
    Pattern str -> '=' : str
    Native  str -> str


fromString :: String -> StringExpr
fromString str = case str of
    "List"               -> List
    "Tuple"              -> Tuple
    "id"                 -> Id
    "Grouped"            -> Grouped
    'g':'e':'t':' ':name -> Get     name
    '=':pat              -> Pattern pat
    '`':'`':'`':_        -> Native  str
    _                    -> Expr    str

