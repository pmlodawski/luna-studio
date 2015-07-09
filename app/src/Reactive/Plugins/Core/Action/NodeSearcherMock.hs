{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.NodeSearcherMock where

import           Prelude       hiding       ( mapM_, forM_ )
import           Control.Lens
import           Control.Applicative
import           Data.Default
import           Data.Maybe
import           Data.List
import           Data.Monoid
import           Data.Function
import           Data.Dynamic
import           System.Mem

import JS.NodeSearcher
import           Utils.Searcher

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy             ( Text )


data EntryType = Function | Module deriving (Show, Eq)

data Entry =   Entry { _tpe :: EntryType, _name :: Text } deriving (Show, Eq)

makeLenses ''Entry

instance Nameable Entry where
    name (Entry _ n) = n

typeToJS Function = "function"
typeToJS Module   = "module"



getItems :: Text -> [Entry]
getItems ""          = [Entry Module "Std", Entry Module "Math", Entry Module "Array"]
getItems "Std"       = [Entry Function "puts", Entry Function "gets"]
getItems "Array"     = [Entry Function "copyWithin ", Entry Function "fill", Entry Function "pop", Entry Function "push", Entry Function "reverse", Entry Function "shift", Entry Function "sort", Entry Function "splice", Entry Function "unshift", Entry Function "concat", Entry Function "includes ", Entry Function "join", Entry Function "slice", Entry Function "toSource ", Entry Function "toString", Entry Function "toLocaleString", Entry Function "indexOf", Entry Function "lastIndexOf", Entry Function "forEach", Entry Function "entries ", Entry Function "every", Entry Function "some", Entry Function "filter", Entry Function "find ", Entry Function "findIndex ", Entry Function "keys ", Entry Function "map", Entry Function "reduce", Entry Function "reduceRight", Entry Function "values "]
getItems "Math"      = [Entry Module "Trig", Entry Function "abs", Entry Function "cbrt", Entry Function "ceil", Entry Function "clz32", Entry Function "exp", Entry Function "expm1", Entry Function "floor", Entry Function "fround", Entry Function "hypot", Entry Function "imul", Entry Function "log", Entry Function "log1p", Entry Function "log10", Entry Function "log2", Entry Function "max", Entry Function "min", Entry Function "pow", Entry Function "random", Entry Function "round", Entry Function "sign", Entry Function "sqrt", Entry Function "toSource", Entry Function "trunc"]
getItems "Math.Trig" = [Entry Function "acos", Entry Function "acosh", Entry Function "asin", Entry Function "asinh", Entry Function "atan", Entry Function "atanh", Entry Function "atan2", Entry Function "cos", Entry Function "cosh", Entry Function "sin", Entry Function "sinh", Entry Function "tan", Entry Function "tanh"]
getItems _           = []


getItemsTree :: Text -> [QueryResult]
getItemsTree prefix =
    fmap makeQueryResult items where
    items = getItems prefix
    makeQueryResult (Entry tpe name) = QueryResult prefix name (prefixWithDot <> name) [] (typeToJS tpe)
    prefixWithDot = if prefix == "" then "" else (prefix <> ".")


getItemsSearch :: Text -> [QueryResult]
getItemsSearch expr
    | (Text.length expr > 0) && (Text.last expr == '.') = getItemsTree (Text.init expr)
    | otherwise                                         = fmap transformMatch $ findSuggestions items [] query
    where
        (prefixWithDot, query) = Text.breakOnEnd "." expr
        prefix = case prefixWithDot of
            "" -> ""
            t  -> Text.init t
        items = getItems prefix
        transformMatch m = case m of
            ExactMatch (Entry tpe name) -> QueryResult prefix name (prefixWithDot <> name) [Highlight 0 (fromIntegral $ Text.length name)] (typeToJS tpe)
            SubstringMatch (Entry tpe name) sm -> QueryResult prefix name (prefixWithDot <> name) (fmap toHighlight sm) (typeToJS tpe)

--            AliasMatch

toHighlight :: Submatch -> Highlight
toHighlight (Submatch s l) = Highlight (fromIntegral s) (fromIntegral l)
