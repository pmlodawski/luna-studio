{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module UI.Widget.LabeledTextBox where

import           Utils.PreludePlus
import           Utils.Vector
import qualified Data.Text.Lazy                as Text

import           Data.JSString.Text            (lazyTextToJSString)
import           GHCJS.Marshal.Pure            (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types                   (JSString, JSVal)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.LabeledTextBox  as Model
import qualified Reactive.State.UIRegistry     as UIRegistry

import           UI.Generic                    (whenChanged)
import qualified UI.Generic                    as UI
import qualified UI.Registry                   as UI
import           UI.Widget                     (UIWidget (..))
import qualified UI.Widget                     as Widget

newtype LabeledTextBox = LabeledTextBox JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget LabeledTextBox

foreign import javascript safe "new LabeledWidget($1, $2, $3, true)" create'   :: Int            -> Double -> Double -> IO LabeledTextBox
foreign import javascript safe "$1.setLabel($2)"                     setLabel' :: LabeledTextBox -> JSString         -> IO ()

create :: WidgetId -> Model.LabeledTextBox -> IO LabeledTextBox
create oid model = do
    textBox      <- create' oid (model ^. Model.size . x) (model ^. Model.size . y)
    setLabel       model textBox
    UI.setWidgetPosition (model ^. widgetPosition) textBox
    return textBox

setLabel :: Model.LabeledTextBox -> LabeledTextBox -> IO ()
setLabel model textBox = setLabel' textBox $ lazyTextToJSString $ model ^. Model.label

instance UIDisplayObject Model.LabeledTextBox where
    createUI parentId id model = do
        textBox   <- create id model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id textBox
        Widget.add textBox parent

    updateUI id old model = do
        textBox <- UI.lookup id :: IO LabeledTextBox
        whenChanged old model Model.label $ setLabel   model textBox

