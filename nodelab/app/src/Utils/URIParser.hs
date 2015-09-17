module Utils.URIParser where

import Utils.PreludePlus

import GHCJS.DOM.Location        (getHash, Location)
import GHCJS.DOM.Window          (getLocation, Window)
import GHCJS.DOM                 (currentWindow)
import Data.List.Split           (wordsBy)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad             (guard)

getHashParts :: MaybeT IO [String]
getHashParts  = do
    window   <- MaybeT currentWindow
    location <- MaybeT $ getLocation window
    hash     <- lift $ getHash location
    return $ wordsBy (== '/') hash

getProjectName :: IO (Maybe String)
getProjectName  = runMaybeT $ do
    parts <- getHashParts
    guard $ length parts >= 3
    guard $ parts !! 1 == "projects"
    return $ parts !! 2
