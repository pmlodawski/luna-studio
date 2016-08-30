module Empire.Utils.TextResult where

import           Prologue
import qualified Data.Text.Lazy               as Text

import           Empire.API.Data.DefaultValue (Value (..))


showLength :: [a] -> String
showLength list = show len <> if exceed then " or more" else "" where
    (len, exceed) = limitedLen list

maxLen = 10000000

limitedLen :: [a] -> (Integer, Bool)
limitedLen = limitedLen' 0 where
    limitedLen' :: Integer -> [a] -> (Integer, Bool)
    limitedLen' acc []     = (acc, False)
    limitedLen' acc (x:xs) = if acc < maxLen
                                then limitedLen' (acc + 1) xs
                                else (acc, True)

nodeValueToText :: Value -> Text
nodeValueToText w = Text.pack $ case w of
    IntValue       v -> show v
    DoubleValue    v -> show v
    BoolValue      v -> show v
    StringValue    v -> "\"" <> (if length v > 10 then take 10 v <> "..." else v) <> "\""
    IntList        v -> "Vector ["  <> showLength v <> "]"
    DoubleList     v -> "Vector ["  <> showLength v <> "]"
    BoolList       v -> "Vector ["  <> showLength v <> "]"
    StringList     v -> "Vector ["  <> showLength v <> "]"
    DoublePairList v -> "Vector2 [" <> showLength v <> "]"
    IntPairList    v -> "Vector2 [" <> showLength v <> "]"
    Histogram      v -> "Hist ["    <> showLength v <> "]"
    Image         {} -> "Image"
    Graphics       _ -> "Graphics"
    _                -> "(unknow type)"
