{-# LANGUAGE OverloadedStrings #-}

module Utils.MockHelper where

import           Utils.PreludePlus

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

import qualified Object.Node as Node

portsMaxin  = 9
portsMaxOut = 9

getInputPortsNr  expr = (ord (head expr) - ord '1' + 1) `mod` (portsMaxin + 1)
getOutputPortsNr expr = 1 + (ord (fromMaybe '1' $ listToMaybe (tail expr)) - ord '1') `mod` portsMaxOut

tryFormat :: Text -> (Int, Int)
tryFormat expr = if   head pref == '#'
                 then (getInputPortsNr lastTwo, getOutputPortsNr lastTwo)
                 else (0, 1)
    where
    pref    = Text.unpack $ Text.takeEnd 3 expr
    lastTwo = Text.unpack $ Text.takeEnd 2 expr

createPorts :: Text -> Node.Ports
createPorts expr = Node.createPorts inputPortsNum outputPortsNum where
    (inputPortsNum, outputPortsNum) = case lookup expr knownFunctions of
        Just ports -> ports
        Nothing    -> tryFormat expr

knownFunctions :: [(Text, (Int, Int))]
knownFunctions  = [ ("+",        (2, 1))
                  , ("-",        (2, 1))
                  , ("*",        (2, 1))
                  , ("/",        (2, 1))
                  , (">",        (2, 1))
                  , ("==",       (2, 1))
                  , ("<",        (2, 1))
                  , ("<=",       (2, 1))
                  , (">=",       (2, 1))
                  , ("++",       (2, 1))
                  , ("toString", (1, 1))
                  , ("truncate", (1, 1))
                  , ("round",    (1, 1))
                  , ("floor",    (1, 1))
                  , ("celing",   (1, 1))
                  ]
