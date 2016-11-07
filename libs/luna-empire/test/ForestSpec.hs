module ForestSpec (spec) where

import Prelude

import Data.List (delete, find, partition)
import Data.Tree

import Test.Hspec (Spec, it, describe)


type NodeId = Int
newtype Breadcrumb = Breadcrumb [BreadcrumbItem]
newtype BreadcrumbItem = BreadcrumbItem NodeId

addToTree :: Breadcrumb -> NodeId -> Forest NodeId -> Forest NodeId
addToTree (Breadcrumb bs) nodeid hierarchy = go bs nodeid hierarchy
    where
        go :: [BreadcrumbItem] -> NodeId -> Forest NodeId -> Forest NodeId
        go [] nodeid forest = Node nodeid [] : forest
        go (BreadcrumbItem b:bs) nodeid forest = case find (\(Node l _) -> b == l) forest of
            Just a@(Node b f) -> Node b (go bs nodeid f) : delete a forest
            _                 -> error "dupa"

navigateTo :: Forest NodeId -> Breadcrumb -> Maybe (Forest NodeId)
navigateTo forest (Breadcrumb breadcrumbs) = go forest nodeIds
    where
      nodeIds = map (\(BreadcrumbItem id) -> id) breadcrumbs
      go :: Forest NodeId -> [NodeId] -> Maybe (Forest NodeId)
      go forest [] = Just forest
      go forest (b:bs) | Just f <- at b forest = go f bs
                       | otherwise = Nothing

at :: NodeId -> Forest NodeId -> Maybe (Forest NodeId)
at nid forest = snd <$> find ((==) nid . fst) topLevel
    where
      topLevel = map (\a -> (rootLabel a, subForest a)) forest

spec :: Spec
spec = describe "forest" $ do
    it "extracts subtree" $ do
        let forest = [Node 1 [Node 4 []], Node 2 [], Node 3 []]
            breadcrumb = Breadcrumb [BreadcrumbItem 1]
        forest `navigateTo` breadcrumb == Just [Node 4 []]
    it "adds node" $ do
        let forest = [Node 1 [], Node 2 [], Node 3 []]
            breadcrumb = Breadcrumb [BreadcrumbItem 1]
        addToTree breadcrumb 4 forest == [Node 1 [Node 4 []], Node 2 [], Node 3 []]
    it "adds node two levels deep" $ do
        let forest = [Node 1 [], Node 2 [], Node 3 []]
            breadcrumb1 = Breadcrumb [BreadcrumbItem 1]
            breadcrumb2 = Breadcrumb [BreadcrumbItem 1, BreadcrumbItem 4]
            res1 = addToTree breadcrumb1 4 forest
            res2 = addToTree breadcrumb2 5 res1
        res1 == [Node 1 [Node 4 []], Node 2 [], Node 3 []] &&
            res2 == [Node 1 [Node 4 [Node 5 []]], Node 2 [], Node 3 []]
    it "adds at the top level" $ do
        let forest = [Node 1 [], Node 2 [], Node 3 []]
        addToTree (Breadcrumb []) 4 forest == [Node 4 [], Node 1 [], Node 2 [], Node 3 []]
