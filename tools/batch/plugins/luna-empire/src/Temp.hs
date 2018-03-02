{-# LANGUAGE DeriveGeneric #-}
module Temp where

import Data.Aeson
import Prologue hiding (id)

data VisNode = VisNode
    { id    :: Int
    , x     :: Int
    , y     :: Int
    , fixed :: Bool
    } deriving (Eq, Generic, Show)

instance ToJSON  VisNode

data VisEdge = VisEdge
    { from   :: Int
    , to     :: Int
    , arrows :: String
    , label  :: String
    } deriving (Eq, Generic, Show)

instance ToJSON  VisEdge

data VisGraph = VisGraph
    { nodes :: [VisNode]
    , edges :: [VisEdge]
    } deriving (Eq, Generic, Show)

instance ToJSON  VisGraph