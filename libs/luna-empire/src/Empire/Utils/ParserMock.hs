module Empire.Utils.ParserMock where

import           Data.Maybe (isJust)
import           Prologue
import           Text.Read  (reads)

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (a : _) = Just a

asInteger :: String -> Maybe (Int, String)
asInteger = safeHead . reads

asString :: String -> Maybe (String, String)
asString = safeHead . reads

asDouble :: String -> Maybe (Double, String)
asDouble = safeHead . reads

isInteger :: String -> Bool
isInteger = isJust . asInteger

isString :: String -> Bool
isString = isJust . asString
