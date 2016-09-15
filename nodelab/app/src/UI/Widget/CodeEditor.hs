{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module UI.Widget.CodeEditor where

import           Utils.PreludePlus
import           Utils.Vector

import qualified Data.JSString         as JSString
import           Data.JSString.Text    (lazyTextToJSString)
import           GHCJS.Marshal.Pure    (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types           (JSString, JSVal)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.CodeEditor as Model

import           UI.Generic            (whenChanged)
import qualified UI.Generic            as UI
import qualified UI.Registry           as UI
import           UI.Widget             (UIWidget)
import qualified UI.Widget             as Widget


newtype CodeEditor = CodeEditor JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget CodeEditor

foreign import javascript unsafe "new CodeEditor($1, $2, $3)" create'        :: Int        -> Double -> Double -> IO CodeEditor
foreign import javascript unsafe "$1.setCode($2)"             setCode'       :: CodeEditor -> JSString         -> IO ()

create :: WidgetId -> Model.CodeEditor -> IO CodeEditor
create oid model = do
    codeEditor <- create' oid (model ^. Model.size . x) (model ^. Model.size . y)
    UI.setWidgetPosition (model ^. widgetPosition) codeEditor
    setCode model codeEditor
    return codeEditor

setCode :: Model.CodeEditor -> CodeEditor -> IO ()
setCode model codeEditor = setCode' codeEditor $ lazyTextToJSString $ model ^. Model.value

instance UIDisplayObject Model.CodeEditor where
    createUI parentId id model = do
        codeEditor <- create id model
        parent     <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id codeEditor
        Widget.add codeEditor parent

    updateUI id old model = do
        codeEditor <- UI.lookup id :: IO CodeEditor
        whenChanged old model Model.value $ setCode  model codeEditor

instance CompositeWidget Model.CodeEditor
instance ResizableWidget Model.CodeEditor where
    resizeWidget = UI.defaultResize
