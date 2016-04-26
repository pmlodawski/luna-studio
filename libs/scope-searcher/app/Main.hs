{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO
import           Control.Lens
import           Control.Monad               (forM_)
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as Text
import           Text.Show.Pretty
import           System.Console.ANSI

import           Text.ScopeSearcher.Item
import           Text.ScopeSearcher.QueryResult
import qualified Text.ScopeSearcher.Scope    as Scope
import qualified Text.ScopeSearcher.Searcher as Searcher

import           MockDataNS

main :: IO ()
main = do
    putStrLn "ScopeSearcher test application"
    withColor Vivid Yellow putStrLn $ ppShow mockDataNS
    withColor Vivid Red    putStrLn "Type :q to quit"
    showHighlights

showHighlights :: IO ()
showHighlights = do
    withColor Vivid Blue putStr "> "
    hFlush stdout
    search <- getLine
    if search == ":q"
        then do
            withColor Dull Red putStrLn "Exiting"
        else do
            showQueryResult $ Text.pack search
            showHighlights


showQueryResult :: Text -> IO ()
showQueryResult searchText = do
    let suggestions = Scope.searchInScope False (mockDataNS ^. items) searchText
    withColor Dull Yellow putStrLn $ ppShow suggestions
    forM_ suggestions $ printQueryResult searchText

withColor :: Show a => ColorIntensity -> Color -> (a -> IO ()) -> a -> IO ()
withColor colorIntensity color printFun output = do
    setSGR [SetColor Foreground colorIntensity color]
    printFun output
    setSGR [SetColor Foreground Vivid White]

printQueryResult :: Text -> QueryResult -> IO ()
printQueryResult search queryResult = do
    putStrLn $ show $ queryResult ^. name
    putStrLn $ show . head $ queryResult ^. highlights
