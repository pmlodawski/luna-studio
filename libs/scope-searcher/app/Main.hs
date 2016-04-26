{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO
import           Control.Lens
import qualified Data.Text.Lazy              as Text
import           Text.Show.Pretty

import           Text.ScopeSearcher.Item
import qualified Text.ScopeSearcher.Scope    as Scope
import qualified Text.ScopeSearcher.Searcher as Searcher

import           MockDataNS

main :: IO ()
main = do
    putStrLn "ScopeSearcher test application"
    putStrLn $ ppShow mockDataNS
    putStrLn "Type :q to quit"
    showHighlights

showHighlights :: IO ()
showHighlights = do
    putStr "> "
    hFlush stdout
    search <- getLine
    if search == ":q"
        then
            putStrLn "Exiting"
        else do
            let suggestions = Scope.searchInScope False (mockDataNS ^. items) $ Text.pack search
            putStrLn $ ppShow suggestions
            showHighlights

