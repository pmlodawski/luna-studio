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
import qualified Data.Map as Map
import           Data.Map ( Map )
import           System.Mem

import           JS.NodeSearcher
import           Utils.Searcher

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy             ( Text )


data LunaModule = LunaModule { items :: Map Text Item } deriving (Show, Eq)
data Item = Function | Module { itmod :: LunaModule } deriving (Show, Eq)

type Path = [Text]

jsItemType Function    = "function"
jsItemType (Module _)  = "module"

data SearchableItem = SearchableItem { _weight :: Double, _path :: Text, _name :: Text, _tpe :: Item } deriving (Eq, Show)

instance Nameable SearchableItem where
    name (SearchableItem _ _ n _) = n

instance Weightable SearchableItem where
    weight (SearchableItem w _ _ _) = w

searchableItems' :: Double -> Text -> LunaModule -> [SearchableItem]
searchableItems' weight prefix (LunaModule it) = Map.foldMapWithKey addItems it where
    addItems :: Text -> Item -> [SearchableItem]
    addItems name i@Function   = [SearchableItem weight prefix name i]
    addItems name i@(Module m) = [SearchableItem weight prefix name i] ++ (searchableItems' (weight*0.9) (nameWithPrefix prefix name) m)
    nameWithPrefix "" name = name
    nameWithPrefix prefix name = prefix <> "." <> name

searchableItems :: LunaModule -> [SearchableItem]
searchableItems = searchableItems' 1.0 ""



moduleByPath :: LunaModule -> Path -> Maybe LunaModule
moduleByPath m []    = Just m
moduleByPath m [""]  = Just m
moduleByPath m [x] = case (Map.lookup x (items m)) of
    Just (Module m) -> Just m
    _               -> Nothing
moduleByPath m (x:xs) = case (Map.lookup x (items m)) of
    Just (Module m) -> moduleByPath m xs
    _               -> Nothing

pathFromText :: Text -> [Text]
pathFromText = Text.splitOn "."

appendPath :: Text -> Text -> Text
appendPath "" n = n
appendPath p  n = p <> "." <> n


itemsInScope :: LunaModule -> Text -> [SearchableItem]
itemsInScope root path = case moduleByPath root (pathFromText path) of
    Just items -> searchableItems items
    Nothing    -> []

searchInScope :: LunaModule -> Text -> [QueryResult]
searchInScope root expr
    | (Text.length expr > 0) && (Text.last expr == '.') = maybe [] (displayModuleItems) scope
    | otherwise                                         = fmap transformMatch $ findSuggestions items query
    where
        (prefixWithDot, query) = Text.breakOnEnd "." expr
        prefix = case prefixWithDot of
            "" -> ""
            t  -> Text.init t
        scope = moduleByPath root $ pathFromText prefix
        items = maybe [] searchableItems scope
        transformMatch (Match score (SearchableItem _ path name tpe) sm) = QueryResult path name (appendPath path name) (fmap toHighlight sm) (jsItemType tpe)
        displayModuleItems (LunaModule it) = fmap di $ Map.toList it
        di  (name, t)  = QueryResult "" name name [] (jsItemType t)



moduleItems :: LunaModule -> Text -> [QueryResult]
moduleItems root path = fmap toQueryResult $ items where
    toQueryResult :: (Text, Item) -> QueryResult
    toQueryResult (name, t)  = QueryResult path name (appendPath path name) [] (jsItemType t)
    items :: [(Text, Item)]
    items = case moduleByPath root (pathFromText path) of
        Just (LunaModule it) -> Map.toList it
        Nothing              -> []



mockArray   = Module $ LunaModule $ Map.fromList [("inspect", Function),("to_s", Function),("to_a", Function),("to_h", Function),("to_ary", Function),("frozen?", Function),("eql?", Function),("hash", Function),("at", Function),("fetch", Function),("first", Function),("last", Function),("concat", Function),("push", Function),("pop", Function),("shift", Function),("unshift", Function),("insert", Function),("each", Function),("each_index", Function),("reverse_each", Function),("length", Function),("size", Function),("empty?", Function),("find_index", Function),("index", Function),("rindex", Function),("join", Function),("reverse", Function),("reverse!", Function),("rotate", Function),("rotate!", Function),("sort", Function),("sort!", Function),("sort_by!", Function),("collect", Function),("collect!", Function),("map", Function),("map!", Function),("select", Function),("select!", Function),("keep_if", Function),("values_at", Function),("delete", Function),("delete_at", Function),("delete_if", Function),("reject", Function),("reject!", Function),("zip", Function),("transpose", Function),("replace", Function),("clear", Function),("fill", Function),("include?", Function),("<=>", Function),("slice", Function),("slice!", Function),("assoc", Function),("rassoc", Function),("+", Function),("*", Function),("-", Function),("&", Function),("|", Function),("uniq", Function),("uniq!", Function),("compact", Function),("compact!", Function),("flatten", Function),("flatten!", Function),("count", Function),("shuffle!", Function),("shuffle", Function),("sample", Function),("cycle", Function),("permutation", Function),("combination", Function),("repeated_permutation", Function),("repeated_combination", Function),("product", Function),("take", Function),("take_while", Function),("drop", Function),("drop_while", Function),("bsearch", Function),("pack", Function),("pretty_print", Function),("pretty_print_cycle", Function),("shelljoin", Function)]
mockInteger = Module $ LunaModule $ Map.fromList [("integer?", Function),("odd?", Function),("even?", Function),("upto", Function),("downto", Function),("times", Function),("succ", Function),("next", Function),("pred", Function),("chr", Function),("ord", Function),("to_i", Function),("to_int", Function),("floor", Function),("ceil", Function),("truncate", Function),("round", Function),("gcd", Function),("lcm", Function),("gcdlcm", Function),("numerator", Function),("denominator", Function),("to_r", Function),("rationalize", Function)]
mockIO      = Module $ LunaModule $ Map.fromList [("reopen", Function),("print", Function),("putc", Function),("puts", Function),("printf", Function),("each", Function),("each_line", Function),("each_byte", Function),("each_char", Function),("each_codepoint", Function),("lines", Function),("bytes", Function),("chars", Function),("codepoints", Function),("syswrite", Function),("sysread", Function),("fileno", Function),("to_i", Function),("to_io", Function),("fsync", Function),("fdatasync", Function),("sync", Function),("sync=", Function),("lineno", Function),("lineno=", Function),("readlines", Function),("read_nonblock", Function),("write_nonblock", Function),("readpartial", Function),("read", Function),("write", Function),("gets", Function),("readline", Function),("getc", Function),("getbyte", Function),("readchar", Function),("readbyte", Function),("ungetbyte", Function),("ungetc", Function),("<<", Function),("flush", Function),("tell", Function),("seek", Function),("rewind", Function),("pos", Function),("pos=", Function),("eof", Function),("eof?", Function),("close_on_exec?", Function),("close_on_exec=", Function),("close", Function),("closed?", Function),("close_read", Function),("close_write", Function),("isatty", Function),("tty?", Function),("binmode", Function),("binmode?", Function),("sysseek", Function),("advise", Function),("ioctl", Function),("fcntl", Function),("pid", Function),("inspect", Function),("external_encoding", Function),("internal_encoding", Function),("set_encoding", Function),("autoclose?", Function),("autoclose=", Function),("stat", Function),("raw", Function),("raw!", Function),("cooked", Function),("cooked!", Function),("getch", Function),("echo=", Function),("echo?", Function),("noecho", Function),("winsize", Function),("winsize=", Function),("iflush", Function),("oflush", Function),("ioflush", Function)]


mockMD5     = Module $ LunaModule $ Map.fromList [("base64digest", Function),("digest", Function),("file", Function),("hexdigest", Function),("digest_length", Function),("inspect", Function)]
mockSHA1    = Module $ LunaModule $ Map.fromList [("reset", Function),("update", Function),("block_length", Function),("digest_length", Function),("inspect", Function)]
mockDigest  = Module $ LunaModule $ Map.fromList [("hexencode", Function), ("MD5", mockMD5), ("SHA1", mockSHA1)]

mockData    = LunaModule $ Map.fromList [("IO", mockIO), ("Integer", mockInteger), ("Array", mockArray), ("Digest", mockDigest)]

getItemsSearch :: Text -> [QueryResult]
getItemsSearch expr = searchInScope mockData expr

getItemsTree :: Text -> [QueryResult]
getItemsTree prefix = moduleItems mockData prefix


-- data EntryType = Function | Module deriving (Show, Eq)
--
-- data Entry =   Entry { _tpe :: EntryType, _name :: Text } deriving (Show, Eq)
--
-- makeLenses ''Entry
--
-- instance Nameable Entry where
--     name (Entry _ n) = n
--
-- typeToJS Function = "function"
-- typeToJS Module   = "module"
--
--
--
-- getItems :: Text    -> [Entry]
-- getItems ""          = [Entry Module "Std", Entry Module "Math", Entry Module "Array"]
-- getItems "Std"       = [Entry Function "puts", Entry Function "gets"]
-- getItems "Array"     = [Entry Function "copyWithin ", Entry Function "fill", Entry Function "pop", Entry Function "push", Entry Function "reverse", Entry Function "shift", Entry Function "sort", Entry Function "splice", Entry Function "unshift", Entry Function "concat", Entry Function "includes ", Entry Function "join", Entry Function "slice", Entry Function "toSource ", Entry Function "toString", Entry Function "toLocaleString", Entry Function "indexOf", Entry Function "laistIndexOf", Entry Function "forEach", Entry Function "entries ", Entry Function "every", Entry Function "some", Entry Function "filter", Entry Function "find ", Entry Function "findIndex ", Entry Function "keys ", Entry Function "map", Entry Function "reduce", Entry Function "reduceRight", Entry Function "values "]
-- getItems "Math"      = [Entry Module "Trig", Entry Function "abs", Entry Function "cbrt", Entry Function "ceil", Entry Function "clz32", Entry Function "exp", Entry Function "expm1", Entry Function "floor", Entry Function "fround", Entry Function "hypot", Entry Function "imul", Entry Function "log", Entry Function "log1p", Entry Function "log10", Entry Function "log2", Entry Function "max", Entry Function "min", Entry Function "pow", Entry Function "random", Entry Function "round", Entry Function "sign", Entry Function "sqrt", Entry Function "toSource", Entry Function "trunc"]
-- getItems "Math.Trig" = [Entry Function "acos", Entry Function "acosh", Entry Function "asin", Entry Function "asinh", Entry Function "atan", Entry Function "atanh", Entry Function "atan2", Entry Function "cos", Entry Function "cosh", Entry Function "sin", Entry Function "sinh", Entry Function "tan", Entry Function "tanh"]
-- getItems _           = []
--
--
-- getItemsTree :: Text -> [QueryResult]
-- getItemsTree prefix =
--     fmap makeQueryResult items where
--     items = getItems prefix
--     makeQueryResult (Entry tpe name) = QueryResult prefix name (prefixWithDot <> name) [] (typeToJS tpe)
--     prefixWithDot = if prefix == "" then "" else (prefix <> ".")
--
--
-- getItemsSearch :: Text -> [QueryResult]
-- getItemsSearch expr
--     | (Text.length expr > 0) && (Text.last expr == '.') = getItemsTree (Text.init expr)
--     | otherwise                                         = fmap transformMatch $ findSuggestions items [] query
--     where
--         (prefixWithDot, query) = Text.breakOnEnd "." expr
--         prefix = case prefixWithDot of
--             "" -> ""
--             t  -> Text.init t
--         items = getItems prefix
--         transformMatch m = case m of
--             ExactMatch (Entry tpe name) -> QueryResult prefix name (prefixWithDot <> name) [Highlight 0 (fromIntegral $ Text.length name)] (typeToJS tpe)
--             SubstringMatch (Entry tpe name) sm -> QueryResult prefix name (prefixWithDot <> name) (fmap toHighlight sm) (typeToJS tpe)

--            AliasMatch

toHighlight :: Submatch -> Highlight
toHighlight (Submatch s l) = Highlight (fromIntegral s) (fromIntegral l)
