{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.NodeSearcher.Scope where

import           Utils.PreludePlus                                  hiding (Item)

import           Data.Map                                           (Map)
import qualified Data.Map                                           as Map
import           GHC.Exts                                           (sortWith)

import           JS.NodeSearcher

import           Data.Text.Lazy                                     (Text)
import qualified Data.Text.Lazy                                     as Text

import           Reactive.Plugins.Core.Action.NodeSearcher.Searcher

import           Empire.API.Data.NodeSearcher                       (Item (..), LunaModule (..))
import qualified Empire.API.Data.NodeSearcher                       as NS

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
moduleByPath m [x] = case (Map.lookup x (m ^. NS.items)) of
    Just (Module m) -> Just m
    _               -> Nothing
moduleByPath m (x:xs) = case (Map.lookup x (m ^. NS.items)) of
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
    | (Text.length expr > 0) && (Text.last expr == '.') = maybe [] displayModuleItems scope
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
        Just (LunaModule it) -> sortWith fst $ Map.toList it
        Nothing              -> []

toHighlight :: Submatch -> Highlight
toHighlight (Submatch s l) = Highlight (fromIntegral s) (fromIntegral l)
