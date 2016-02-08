module JS.Terminal where

import           Utils.PreludePlus
import           GHCJS.Foreign
import           GHCJS.Types (JSString)
import           Data.JSString (pack)

foreign import javascript safe "app.writeToTerminal($1)"
    writeToTerminal' :: JSString -> IO ()

writeToTerminal :: String -> IO ()
writeToTerminal = writeToTerminal' . pack
