module Reactive.Commands.Node.TextResult
    ( nodeValueToText
    ) where

import qualified Data.Text.Lazy               as Text
import           Utils.PreludePlus

import           Empire.API.Data.DefaultValue (Value (..))

nodeValueToText :: Value -> Text
nodeValueToText w = Text.pack $ case w of
    IntValue       v -> show v
    DoubleValue    v -> show v
    BoolValue      v -> show v
    StringValue    v -> "\"" <> (if (length v > 10) then (take 10 v) <> "..." else v) <> "\""
    IntList        v -> "Vector ["  <> (show $ length v) <> "]"
    DoubleList     v -> "Vector ["  <> (show $ length v) <> "]"
    BoolList       v -> "Vector ["  <> (show $ length v) <> "]"
    StringList     v -> "Vector ["  <> (show $ length v) <> "]"
    DoublePairList v -> "Vector2 [" <> (show $ length v) <> "]"
    IntPairList    v -> "Vector2 [" <> (show $ length v) <> "]"
    Histogram      v -> "Hist ["    <> (show $ length v) <> "]"
    Image      _ _ _ -> "Image"
    Graphics       _ -> "Graphics"
    otherwise        -> "(unknow type)"
