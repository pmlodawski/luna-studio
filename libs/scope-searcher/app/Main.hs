{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO
import           Control.Lens
import           Control.Monad               (forM_, when)
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
    withColor Vivid Green  putStrLn "Press enter to quit."
    showHighlights

showHighlights :: IO ()
showHighlights = do
    withColor Vivid Blue putStr "> "
    hFlush stdout
    input <- getLine
    if null input
        then do
            withColor Vivid Red putStrLn "Leaving ScopeSearcher."
        else do
            let debug = head input == '!'
                search = if debug then tail input else input
            showQueryResults debug $ Text.pack search
            showHighlights

showQueryResults :: Bool -> Text -> IO ()
showQueryResults debug searchText = do
    let suggestions = Scope.searchInScope False (mockDataNS ^. items) searchText
    when debug $ withColor Dull Yellow putStrLn $ ppShow suggestions
    forM_ suggestions showQueryResult

showQueryResult :: QueryResult -> IO ()
showQueryResult queryResult = showHighlight 0 (queryResult ^. highlights) $ Text.unpack (queryResult ^. name)

showHighlight :: Int -> [Highlight] -> String -> IO ()
showHighlight _ [] str = withColor Dull White putStrLn str
showHighlight pos ((Highlight start len):highlights) str = do
    let hlPos = start - pos
        (normal, remainder) = splitAt hlPos str
        (highlighted, rest) = splitAt len remainder
    withColor Dull  White putStr normal
    withColor Vivid Red   putStr highlighted
    showHighlight (hlPos + len) highlights rest

withColor :: Show a => ColorIntensity -> Color -> (a -> IO ()) -> a -> IO ()
withColor colorIntensity color printFun output = do
    setSGR [SetColor Foreground colorIntensity color]
    printFun output
    setSGR [SetColor Foreground Vivid White]
