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
import Reactive.Plugins.Core.Action.NodeSearcherSearch

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy             ( Text )



data Entry = Function { _name :: Text }
           | Module   { _name :: Text } deriving (Show, Eq)

makeLenses ''Entry

instance Nameable Entry where
    name (Function n ) = n
    name (Module   n ) = n



getItems :: Text -> [Entry]
getItems ""          = [Module "Std", Module "Math", Module "Array"]
getItems "Std"       = [Function "puts", Function "gets"]
getItems "Array"     = [Function "copyWithin ", Function "fill", Function "pop", Function "push", Function "reverse", Function "shift", Function "sort", Function "splice", Function "unshift", Function "concat", Function "includes ", Function "join", Function "slice", Function "toSource ", Function "toString", Function "toLocaleString", Function "indexOf", Function "lastIndexOf", Function "forEach", Function "entries ", Function "every", Function "some", Function "filter", Function "find ", Function "findIndex ", Function "keys ", Function "map", Function "reduce", Function "reduceRight", Function "values "]
getItems "Math"      = [Module "Trig", Function "abs", Function "cbrt", Function "ceil", Function "clz32", Function "exp", Function "expm1", Function "floor", Function "fround", Function "hypot", Function "imul", Function "log", Function "log1p", Function "log10", Function "log2", Function "max", Function "min", Function "pow", Function "random", Function "round", Function "sign", Function "sqrt", Function "toSource", Function "trunc"]
getItems "Math.Trig" = [Function "acos", Function "acosh", Function "asin", Function "asinh", Function "atan", Function "atanh", Function "atan2", Function "cos", Function "cosh", Function "sin", Function "sinh", Function "tan", Function "tanh"]
getItems _           = []


getItemsTree :: Text -> [QueryResult]
getItemsTree prefix =
    fmap makeQueryResult items where
    items = getItems prefix
    makeQueryResult (Function name) = QueryResult prefix name (prefixWithDot <> name) [] "function"
    makeQueryResult (Module   name) = QueryResult prefix name (prefixWithDot <> name) [] "module"
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
            ExactMatch (Function name) -> QueryResult prefix name (prefixWithDot <> name) [Highlight 0 (fromIntegral $ Text.length name)] "function"
            SubstringMatch (Function name) sm -> QueryResult prefix name (prefixWithDot <> name) (fmap toHighlight sm) "function"
            ExactMatch (Module name) -> QueryResult prefix name (prefixWithDot <> name) [Highlight 0 (fromIntegral $ Text.length name)] "module"
            SubstringMatch (Module name) sm -> QueryResult prefix name (prefixWithDot <> name) (fmap toHighlight sm) "module"
        -- prefixWithDot = if prefix == "" then "" else (prefix <> ".")

--            AliasMatch

toHighlight :: Submatch -> Highlight
toHighlight (Submatch s l) = Highlight (fromIntegral s) (fromIntegral l)


-- mockQueryResults :: [JS.NodeSearcher.QueryResult]
-- mockQueryResults =
--     [
--      (JS.NodeSearcher.QueryResult "Math" "sqrt" "Math.sqrt" [JS.NodeSearcher.Highlight 1 3] "function"),
--      (JS.NodeSearcher.QueryResult "Math" "Trig" "Math.Trig" [JS.NodeSearcher.Highlight 1 2] "module"),
--      (JS.NodeSearcher.QueryResult "Math.Trig" "sin" "Math.Trig.sin" [JS.NodeSearcher.Highlight 2 3] "function")
--     ]
