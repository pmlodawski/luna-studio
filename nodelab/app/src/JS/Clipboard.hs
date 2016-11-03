module JS.Clipboard
    ( registerCopyCallback
    , registerCutCallback
    , registerPasteCallback
    ) where

import           Data.JSString.Text        (lazyTextFromJSString)
import           Event.Clipboard
import           GHCJS.Foreign.Callback
import           GHCJS.Types
import           Reactive.Commands.Command (performIO)
import           Utils.PreludePlus

foreign import javascript safe "window.addEventListener('copy', function(e){$1(e); console.log('copy', e); e.preventDefault();})"
    registerCopyCallback' :: Callback (JSVal -> IO ()) -> IO ()

registerCopyCallback :: (JSVal -> IO ()) -> IO (IO ())
registerCopyCallback callback = do
    wrappedCallback <- asyncCallback1 callback
    registerCopyCallback' wrappedCallback
    return $ return ()

foreign import javascript safe "window.addEventListener('cut', function(e){$1(e); console.log('cut', e); e.preventDefault();})"
    registerCutCallback' :: Callback (JSVal -> IO ()) -> IO ()

registerCutCallback :: (JSVal -> IO ()) -> IO (IO ())
registerCutCallback callback = do
    wrappedCallback <- asyncCallback1 callback
    registerCutCallback' wrappedCallback
    return $ return ()

foreign import javascript safe "window.addEventListener('paste', function(e){$1(e.clipboardData.getData('text/plain')); console.log('paste', e); e.preventDefault();})"
    registerPasteCallback' :: Callback (JSVal -> IO ()) -> IO ()

registerPasteCallback :: (JSVal -> IO ()) -> IO (IO ())
registerPasteCallback callback = do
    wrappedCallback <- asyncCallback1 callback
    registerPasteCallback' wrappedCallback
    return $ return ()

-- foreign import javascript safe "$r = $1"
--     getClipboardData' :: JSVal -> JSString
--
-- getClipboardData :: Text
-- getClipboardData cbd = do
--   lazyTextFromJSString $ getClipboardData' (cbd ^. Event.Clipboard.jsval)
