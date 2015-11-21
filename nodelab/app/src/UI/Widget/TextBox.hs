{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Widget.TextBox where

import           Utils.PreludePlus
import           Utils.Vector

import           GHCJS.Types        (JSVal, JSString)
import           GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import           Data.JSString.Text (lazyTextToJSString)

import           UI.Widget          (UIWidget(..))
import qualified Object.Widget.TextBox as Model
import qualified UI.Widget            as Widget
import qualified UI.Registry          as UI
import qualified UI.Generic           as UI
import           Object.Widget
import           Object.UITypes

newtype TextBox = TextBox JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget TextBox

foreign import javascript unsafe "new TextBox($1, $2, $3)"  create'           :: Int     -> Double -> Double -> IO TextBox
foreign import javascript unsafe "$1.setLabel($2)"          setLabel'         :: TextBox -> JSString         -> IO ()
foreign import javascript unsafe "$1.setValueLabel($2)"     setValueLabel'    :: TextBox -> JSString         -> IO ()
-- foreign import javascript unsafe "$1.setFocus($2)"          setFocus'         :: TextBox -> Bool             -> IO ()
foreign import javascript unsafe "$1.startEditing($2)"      startEditing'     :: TextBox -> JSString         -> IO ()
foreign import javascript unsafe "$1.doneEditing()"         doneEditing'      :: TextBox                     -> IO ()


create :: WidgetId -> Model.TextBox -> IO TextBox
create oid model = do
    textBox      <- create' oid (model ^. Model.size . x) (model ^. Model.size . y)
    setLabel       model textBox
    setValueLabel  model textBox
    UI.setWidgetPosition (model ^. widgetPosition) textBox
    return textBox

setLabel :: Model.TextBox -> TextBox -> IO ()
setLabel model textBox = setLabel' textBox $ lazyTextToJSString $ model ^. Model.label

setValueLabel :: Model.TextBox -> TextBox -> IO ()
setValueLabel model textBox = setValueLabel' textBox $ lazyTextToJSString $ model ^. Model.value

-- setFocus :: Model.TextBox -> TextBox -> IO ()
-- setFocus model slider = setFocus' slider $ model ^. Model.focused

ifChanged :: (Eq b) => a -> a -> Lens' a b -> IO () -> IO ()
ifChanged old new get action = if (old ^. get) /= (new ^. get) then action
                                                               else return ()

instance UIDisplayObject Model.TextBox where
    createUI parentId id model = do
        textBox   <- create id model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id textBox
        Widget.add textBox parent

    updateUI id old model = do
        textBox <- UI.lookup id :: IO TextBox

        ifChanged old model Model.label $ setLabel       model textBox
        ifChanged old model Model.value $ setValueLabel  model textBox

        ifChanged old model Model.isEditing $ do
            if old ^. Model.isEditing then doneEditing' textBox
                                      else do
                                          startEditing'  textBox $ lazyTextToJSString $ model ^. Model.value
                                          setValueLabel' textBox ""


