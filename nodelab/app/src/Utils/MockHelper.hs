module Utils.MockHelper where

import           Utils.PreludePlus

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

import qualified Object.Node as Node

portsMaxin  = 9
portsMaxOut = 9

getInputPortsNr  expr = (ord (head expr) - ord '1' + 1) `mod` (portsMaxin + 1)
getOutputPortsNr expr = 1 + (ord (fromMaybe '1' $ listToMaybe (tail expr)) - ord '1') `mod` portsMaxOut

createPorts :: Text -> Node.Ports
createPorts expr = Node.createPorts inputPortsNum outputPortsNum where
    portsString     = Text.unpack $ Text.takeEnd 2 expr
    inputPortsNum   = getInputPortsNr  portsString
    outputPortsNum  = getOutputPortsNr portsString

createPortsMay :: Maybe Text -> Node.Ports
createPortsMay exprMay = createPorts $ fromMaybe (Text.pack "11") exprMay
