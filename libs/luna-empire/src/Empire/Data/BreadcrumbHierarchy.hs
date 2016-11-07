module Empire.Data.BreadcrumbHierarchy (
      BreadcrumbHierarchy
    , empty
    , replaceAt
    , navigateTo
    , addID
    , topLevelIDs
    ) where

import           Prologue hiding (at)

import           Data.List (delete, find)
import           Data.Tree

import           Empire.API.Data.Breadcrumb (Breadcrumb (..), BreadcrumbItem (..))
import           Empire.API.Data.Node       (NodeId)


newtype BreadcrumbHierarchy = BC (Forest NodeId) deriving Show

empty :: BreadcrumbHierarchy
empty = BC []

replaceAt :: Breadcrumb -> BreadcrumbHierarchy -> BreadcrumbHierarchy -> BreadcrumbHierarchy
replaceAt (Breadcrumb bs) (BC forest) (BC hierarchy) = BC $ go bs forest hierarchy
    where
      go :: [BreadcrumbItem] -> Forest NodeId -> Forest NodeId -> Forest NodeId
      go [] newForest _ = newForest
      go (Lambda b:bs) newForest forest = case find (\(Node l _) -> b == l) forest of
          Just a@(Node b f) -> Node b (go bs newForest f) : delete a forest
          -- _                 -> TODO this cannot occur since we check beforehand that
          -- it is safe to navigate to breadcrumb in breadcrumbHierarchy
          -- should be handled better, though

navigateTo :: BreadcrumbHierarchy -> Breadcrumb -> Maybe BreadcrumbHierarchy
navigateTo (BC forest) (Breadcrumb breadcrumbs) = BC <$> go forest nodeIds
    where
      nodeIds = map (\(Lambda id) -> id) breadcrumbs
      go :: Forest NodeId -> [NodeId] -> Maybe (Forest NodeId)
      go forest [] = Just forest
      go forest (b:bs) | Just f <- at b forest = go f bs
                       | otherwise = Nothing

at :: NodeId -> Forest NodeId -> Maybe (Forest NodeId)
at nid forest = snd <$> find ((==) nid . fst) topLevel
    where
      topLevel = map (\a -> (rootLabel a, subForest a)) forest

addID :: NodeId -> BreadcrumbHierarchy -> BreadcrumbHierarchy
addID nodeid (BC hierarchy) = BC $ Node nodeid [] : hierarchy

topLevelIDs :: BreadcrumbHierarchy -> [NodeId]
topLevelIDs (BC f) = map (\(Node nid _) -> nid) f
