module Batch.Value where

import Utils.PreludePlus

data Value = FloatValue Float
           | IntValue Int
           deriving (Eq, Show)
