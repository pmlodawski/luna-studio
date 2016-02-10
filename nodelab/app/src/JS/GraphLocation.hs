{-# LANGUAGE OverloadedStrings #-}
module JS.GraphLocation (
    saveLocation,
    loadLocation
) where

import           Data.Aeson             (encode, decode)
import           Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.JSString          as JSString
import           JavaScript.Web.Storage (getItem, localStorage, setItem)
import           Utils.PreludePlus

import qualified Batch.Workspace        as Workspace

key = "lastLocation"

saveLocation :: Workspace.UIGraphLocation -> IO ()
saveLocation location = do
    let payload = JSString.pack . ByteString.unpack $ encode location
    setItem key payload localStorage

loadLocation :: IO (Maybe (Workspace.UIGraphLocation))
loadLocation = do
    payload <- getItem key localStorage
    return $ case payload of
        Just payload -> decode $ ByteString.pack $ JSString.unpack payload
        Nothing      -> Nothing
