module Batch.Value where

import Utils.PreludePlus

data Value = FloatValue Float
           | IntValue Int
           | StringValue String
           | CharValue Char
           | BoolValue Bool
           deriving (Eq, Show)
