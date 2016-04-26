{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid                    ((<>))
import           Test.HUnit

import           Text.ScopeSearcher.Item
import           Text.ScopeSearcher.QueryResult
import qualified Text.ScopeSearcher.Scope       as Scope
import qualified Text.ScopeSearcher.Searcher    as Searcher

import           MockDataNS


assertMsg :: String -> String -> Int -> String
assertMsg query sugg ind = "'" <> sugg <> "' should be " <> show ind <> " on suggestions list for '" <> query <> "' query"

testSearchApp :: Test
testSearchApp = TestCase $ do
    assertEqual (assertMsgApp "APP"    0) (QueryResult ""     "app"    "app"    [ Highlight 0 3 ] "function") $ suggestions !! 0
    assertEqual (assertMsgApp "APPend" 1) (QueryResult "List" "append" "append" [ Highlight 0 3 ] "function") $ suggestions !! 1
    where suggestions = Scope.searchInScope False mockItemsNS "app"
          assertMsgApp = assertMsg "app"

testSearchCos :: Test
testSearchCos = TestCase $ do
    assertEqual (assertMsgCos "COSh"  0) (QueryResult "Double" "cosh"  "cosh"  [ Highlight 0 3 ] "function") $ suggestions !! 0
    assertEqual (assertMsgCos "COS"   1) (QueryResult "Double" "cos"   "cos"   [ Highlight 0 3 ] "function") $ suggestions !! 1
    assertEqual (assertMsgCos "aCOS"  2) (QueryResult "Double" "acos"  "acos"  [ Highlight 1 3 ] "function") $ suggestions !! 2
    assertEqual (assertMsgCos "aCOSh" 3) (QueryResult "Double" "acosh" "acosh" [ Highlight 1 3 ] "function") $ suggestions !! 3
    assertEqual (assertMsgCos "COnSt" 4) (QueryResult ""       "const" "const" [ Highlight 0 2, Highlight 3 1 ] "function") $ suggestions !! 4
    where suggestions = Scope.searchInScope False mockItemsNS "cos"
          assertMsgCos = assertMsg "cos"

main :: IO Counts
main = do
    runTestTT $ TestList [
          testSearchApp
        , testSearchCos
        ]
