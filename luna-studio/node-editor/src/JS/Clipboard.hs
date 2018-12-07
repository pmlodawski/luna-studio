{-# LANGUAGE JavaScriptFFI #-}

module JS.Clipboard
    ( copyStringToClipboard
    ) where

import           Common.Prelude

foreign import javascript safe "copyToClipboard($1, $2)" copyStringToClipboard :: JSString -> JSString -> IO ()
