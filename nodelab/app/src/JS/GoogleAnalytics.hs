{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.Cursor where

import qualified Data.JSString     as JSString
import           GHCJS.Foreign
import           GHCJS.Types       (JSString)
import           Utils.PreludePlus
import           Utils.Vector

foreign import javascript safe "ga('send','event,$1, $2, $3)" sendEvent' :: JSString -> JSString -> Maybe JSString -> IO ()


data ConnectType = Manual | Pen | AutoConnect
data NodeSearcherType = Open | MouseSelect | SearchSelect
data AddNodeType = Solo | AutoConnect
data Event = BSOD
           | ConnectionLost
           | Connect ConnectType
           | NodeSearcher NodeSearcherType
           | AddNode AddNodeType
           | RemoveNode Int -- Number
           | CreateProject
           | SwitchProject
           | GAOptOut
           | OpenHelp
           | ToggleText

-- URL - numer builda
-- EVENT -
    -- BSOD
    -- Connection lost
    -- JS error *powinny byc w BSOD*
    -- connection z rozroznieniem na sposob (reczny, pen, autoconnect)
    -- NS: wybranie z podziałem na drzewko / searcher
    -- dodanie noda z podzialem na solo / autoconnect
    -- usuniecie noda
    -- stworzenie projektu
    -- zmiana projektu
    -- optout z GA
    -- otwarcie helpa
    -- toggle tekstowej wersji
