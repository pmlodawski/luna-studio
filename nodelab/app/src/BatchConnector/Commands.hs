module BatchConnector.Commands where

import GHCJS.DOM.WebSocket
import System.IO

addNode :: WebSocket -> IO ()
addNode conn = sendString conn "dupa"
