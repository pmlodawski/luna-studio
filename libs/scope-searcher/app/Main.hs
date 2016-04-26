{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO
import           Control.Lens
import           Control.Monad               (forM_, when)
import           Control.Monad.State         (evalStateT, StateT, liftIO, get, put, modify)
import           Data.Default
import           Data.Monoid                 ((<>))
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as Text
import           Text.Show.Pretty
import           System.Console.ANSI

import           Text.ScopeSearcher.Item
import           Text.ScopeSearcher.QueryResult
import qualified Text.ScopeSearcher.Scope    as Scope
import qualified Text.ScopeSearcher.Searcher as Searcher

import           MockDataNS

data Env = Env { _debug       :: Bool
               , _includePath :: Bool
               } deriving (Show)

instance Default Env where
    def = Env False False

makeLenses ''Env

main :: IO ()
main = do
    withColor Vivid Magenta putStrLn "ScopeSearcher test application"
    withColor Vivid Yellow  putStrLn "@     - show mock\n?     - toggle debug\n/     - toggle include path\nenter - quit"
    evalStateT showHighlights def
    withColor Vivid Red putStrLn "Leaving ScopeSearcher."

showMock :: IO ()
showMock = withColor Vivid Green putStrLn $ ppShow mockDataNS

showHighlights :: StateT Env IO ()
showHighlights = do
    input <- liftIO $ getUserInput
    when (not $ null input) $ do
        let shouldToggleDebug       = input == "?"
            shouldToggleIncludePath = input == "/"
            shouldShowMock          = input == "@"
        debugState       <- use debug
        includePathState <- use includePath
        case (shouldToggleDebug, shouldToggleIncludePath, shouldShowMock) of
            (False, False, False) -> liftIO $ showQueryResults debugState includePathState $ Text.pack input
            (_, _, _) -> do
                when shouldToggleDebug       updateDebug
                when shouldToggleIncludePath updateIncludePath
                when shouldShowMock $ liftIO showMock
        showHighlights

updateDebug :: StateT Env IO ()
updateDebug = zoom debug $ do
    modify not
    debug <- get
    liftIO $ withColor Vivid Yellow putStrLn $ "Toggled debug to " <> show debug

updateIncludePath :: StateT Env IO ()
updateIncludePath = zoom includePath $ do
    modify not
    includePath <- get
    liftIO $ withColor Vivid Yellow putStrLn $ "Toggled includePath to " <> show includePath

getUserInput :: IO String
getUserInput = do
    withColor Vivid Blue putStr "> "
    hFlush stdout
    getLine

showQueryResults :: Bool -> Bool -> Text -> IO ()
showQueryResults debug includePath searchText = do
    let suggestions = Scope.searchInScope includePath (mockDataNS ^. items) searchText
    when debug $ withColor Dull Yellow putStrLn $ ppShow suggestions
    forM_ suggestions showQueryResult

showQueryResult :: QueryResult -> IO ()
showQueryResult queryResult = do
    showHighlight 0 (queryResult ^. highlights) $ Text.unpack (queryResult ^. name)

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
