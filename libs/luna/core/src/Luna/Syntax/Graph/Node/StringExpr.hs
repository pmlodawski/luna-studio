---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Syntax.Graph.Node.StringExpr where

import           Flowbox.Prelude



data StringExpr = Expr    { _string :: String }
                | Tuple
                | Get     { _string :: String }
                | List
                | Id
                | Pattern { _string :: String }
                | Native  { _string :: String }
                deriving (Show, Eq, Read)

makeLenses ''StringExpr


instance IsString StringExpr where
    fromString str = case str of
        "List"               -> List
        "Tuple"              -> Tuple
        "id"                 -> Id
        'g':'e':'t':' ':name -> Get     name
        '=':pat              -> Pattern pat
        '`':'`':'`':_        -> Native  str
        _                    -> Expr    str

instance ToString StringExpr where
    toString exprStr = case exprStr of
        Expr    str -> str
        Tuple       -> "Tuple"
        Get     str -> "get " ++ str
        List        -> "List"
        Id          -> "id"
        Pattern str -> '=' : str
        Native  str -> str
