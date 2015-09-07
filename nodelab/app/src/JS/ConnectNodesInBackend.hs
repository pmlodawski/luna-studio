{-# LANGUAGE JavaScriptFFI #-}

module JS.ConnectNodesInBackend where

-- Just a workaround for the time frontend cannot connect nodes visually
import Utils.PreludePlus
import GHCJS.Foreign.Callback
import Data.Int
import GHCJS.Prim
import Batch.Workspace
import BatchConnector.Connection
import BatchConnector.Commands

data ConnectCmd

foreign import javascript unsafe "window.connectNodes = $1"
    setConnectNodeCallback' :: Callback (JSRef ConnectCmd -> IO ()) -> IO ()

foreign import javascript unsafe "$1.srcNode"
    getSrcNode :: JSRef ConnectCmd -> JSRef ()

foreign import javascript unsafe "$1.dstNode"
    getDstNode :: JSRef ConnectCmd -> JSRef ()

foreign import javascript unsafe "$1.srcPorts"
    getSrcPorts :: JSRef ConnectCmd -> JSRef ()

foreign import javascript unsafe "$1.dstPorts"
    getDstPorts :: JSRef ConnectCmd -> JSRef ()

connect :: Workspace -> JSRef ConnectCmd -> IO ()
connect workspace cmd = do
    let srcNode = fromJSInt $ getSrcNode cmd
        dstNode = fromJSInt $ getDstNode cmd
    srcPortsJS <- fromJSArray $ getSrcPorts cmd
    dstPortsJS <- fromJSArray $ getDstPorts cmd

    let srcPorts = fromJSInt <$> srcPortsJS
    let dstPorts = fromJSInt <$> dstPortsJS

    sendMessage $ connectNodes' workspace srcNode srcPorts dstNode dstPorts

setConnectNodeCallback :: Workspace -> IO ()
setConnectNodeCallback workspace = do
    wrappedCallback <- asyncCallback1 $ connect workspace
    setConnectNodeCallback' wrappedCallback
