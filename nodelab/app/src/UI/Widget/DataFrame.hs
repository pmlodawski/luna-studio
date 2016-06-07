{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module UI.Widget.DataFrame where

import           Data.Aeson              (toJSON)
import           GHCJS.Marshal           (toJSVal)
import           GHCJS.Types             (JSVal)
import           Utils.PreludePlus
import           Utils.Vector

import           GHCJS.Marshal.Pure      (PFromJSVal (..), PToJSVal (..))

import           Object.Widget
import qualified Object.Widget.DataFrame as Model

import           UI.Generic              (whenChanged)
import qualified UI.Generic              as UI
import qualified UI.Registry             as UI
import           UI.Widget               (UIWidget)
import qualified UI.Widget               as Widget


newtype DataFrame = DataFrame JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget DataFrame

foreign import javascript safe "new DataFrame($1, $2, $3)"  create'  :: Int     -> Double -> Double -> IO DataFrame
foreign import javascript safe "$1.setData($2, $3)"            setData'   :: DataFrame -> JSVal -> JSVal -> IO ()
foreign import javascript safe "$1.relayout()"           realayout' :: DataFrame          -> IO ()

create :: WidgetId -> Model.DataFrame -> IO DataFrame
create oid model = do
    textBox      <- create' oid (model ^. Model.size . x) (model ^. Model.size . y)
    setData      model textBox
    UI.setWidgetPosition (model ^. widgetPosition) textBox
    return textBox

setData :: Model.DataFrame -> DataFrame -> IO ()
setData model textBox = do
    let h' = toJSON (model ^. Model.headers)
        d' = toJSON (model ^. Model.rows)
    h <- toJSVal h'
    d <- toJSVal d'
    setData' textBox h d

instance UIDisplayObject Model.DataFrame where
    createUI parentId id model = do
        textBox   <- create id model
        parent    <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register id textBox
        Widget.add textBox parent
        realayout' textBox

    updateUI id old model = do
        textBox <- UI.lookup id :: IO DataFrame

        whenChanged old model Model.rows $     setData      model textBox

instance CompositeWidget Model.DataFrame
instance ResizableWidget Model.DataFrame where
    resizeWidget = UI.defaultResize
