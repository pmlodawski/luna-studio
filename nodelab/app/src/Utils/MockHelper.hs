{-# LANGUAGE OverloadedStrings #-}

module Utils.MockHelper where

import           Utils.PreludePlus
import           Object.Port
import           Control.Monad

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

import qualified Object.Node as Node


data PortTypes = PortTypes { _inputs  :: [ValueType]
                           , _outputs :: [ValueType]
                           } deriving (Eq, Show)

makeLenses ''PortTypes

portsMaxin  = 9
portsMaxOut = 9

getInputPortsNr  expr = (ord (head expr) - ord '1' + 1) `mod` (portsMaxin + 1)
getOutputPortsNr expr = 1 + (ord (fromMaybe '1' $ listToMaybe (tail expr)) - ord '1') `mod` portsMaxOut

tryVal :: Text -> Maybe PortTypes
tryVal _ = Just $ PortTypes [] [Float]

tryDef :: Text -> Maybe PortTypes
tryDef expr = case Text.unpack $ Text.take 4 expr of
    "def " -> Just $ PortTypes [] []
    _      -> Nothing

tryFormat :: Text -> Maybe PortTypes
tryFormat expr = case head suf of
    '#' -> Just $ PortTypes (replicate inputPortsNum Float)
                            (replicate outputPortsNum Float)
    _   -> Nothing
    where
    suf            = Text.unpack $ Text.takeEnd 3 expr
    lastTwo        = Text.unpack $ Text.takeEnd 2 expr
    inputPortsNum  = getInputPortsNr lastTwo
    outputPortsNum = getOutputPortsNr lastTwo

tryKnown :: Text -> Maybe PortTypes
tryKnown expr = lookup expr knownFunctions

knownFunctions  = [ ("+",        PortTypes [Float,   Float]  [Float])
                  , ("-",        PortTypes [Float,   Float]  [Float])
                  , ("*",        PortTypes [Float,   Float]  [Float])
                  , ("/",        PortTypes [Float,   Float]  [Float])
                  , (">",        PortTypes [Float,   Float]  [Bool])
                  , ("==",       PortTypes [Float,   Float]  [Bool])
                  , ("<",        PortTypes [Float,   Float]  [Bool])
                  , ("<=",       PortTypes [Float,   Float]  [Bool])
                  , (">=",       PortTypes [Float,   Float]  [Bool])
                  , ("++",       PortTypes [String,  String] [String])
                  , ("toString", PortTypes [Float]  [String])
                  , ("truncate", PortTypes [Float]  [Float])
                  , ("round",    PortTypes [Float]  [Float])
                  , ("floor",    PortTypes [Float]  [Float])
                  , ("celing",   PortTypes [Float]  [Float])
                  ]

createPorts :: Text -> Node.Ports
createPorts expr = Node.createPorts inputs outputs where
    PortTypes inputs outputs = case msum $ ($ expr) <$> [tryKnown, tryDef, tryFormat, tryVal] of
        Just ports -> ports
        Nothing    -> PortTypes [] []
