module Empire.Utils.ParserMock where

import Prologue
import Text.Read  (readMaybe)
import Data.Maybe (isJust)

asInteger :: String -> Maybe Int
asInteger expr = readMaybe expr

asString :: String -> Maybe String
asString expr = readMaybe expr

isInteger :: String -> Bool
isInteger = isJust . asInteger

isString :: String -> Bool
isString = isJust . asString
