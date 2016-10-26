{-# LANGUAGE OverloadedStrings #-}
module JS.GraphLocation (
    saveLocation,
    loadLocation
) where

import           Data.Aeson                 (decode, encode)
import           Data.ByteString.Lazy.Char8 as ByteString
import           Data.JSString              (JSString)
import qualified Data.JSString              as JSString
import           JavaScript.Web.Storage     (getItem, localStorage, setItem)
import           Utils.PreludePlus

import qualified Batch.Workspace            as Workspace



key :: JSString
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
