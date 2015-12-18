{-# LANGUAGE OverloadedStrings #-}

module UI.Widget.Connection where

import           Utils.CtxDynamic          (toCtxDynamic)
import           Utils.PreludePlus
import           Utils.Vector

import           Data.JSString.Text        (lazyTextFromJSString, lazyTextToJSString)
import           GHCJS.DOM.Element         (Element)
import           GHCJS.Marshal.Pure        (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types               (JSString, JSVal)

import           Event.Mouse               (MouseButton (..))
import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Connection  as Model
import           Reactive.Commands.Command (Command, ioCommand, performIO)

import qualified UI.Registry               as UIR
import           UI.Widget                 (UIContainer (..), UIWidget (..))
import           UI.Widget                 (GenericWidget (..))
import qualified UI.Widget                 as UIT

newtype Connection = Connection { unConnection :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget Connection

foreign import javascript unsafe "new Connection($1)"         create'    :: WidgetId   -> IO Connection
foreign import javascript unsafe "$1.setPos($2, $3, $4, $5)"  setPos     :: Connection -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.setVisible($2)"          setVisible :: Connection -> Bool   -> IO ()
foreign import javascript unsafe "$1.setColor($2)"            setColor   :: Connection -> Int    -> IO ()

create :: WidgetId -> Model.Connection -> IO Connection
create id model = do
    connection <- create' id
    setPos     connection (model ^. Model.from . x)  (model ^. Model.from . y)  (model ^. Model.to . x)  (model ^. Model.to . y)
    setVisible connection (model ^. Model.visible)
    setColor   connection (model ^. Model.color)
    return connection

instance UIDisplayObject Model.Connection where
    createUI parentId id model = do
        widget  <- create id model
        parent  <- UIR.lookup parentId :: IO GenericWidget
        UIR.register id widget
        UIT.add widget parent

    updateUI id old model = do
        connection <- UIR.lookup id :: IO Connection
        setPos     connection (model ^. Model.from . x)  (model ^. Model.from . y)  (model ^. Model.to . x)  (model ^. Model.to . y)
        setVisible connection (model ^. Model.visible)
        setColor   connection (model ^. Model.color)

instance UIDisplayObject Model.CurrentConnection where
    createUI = undefined
    updateUI id _ model = do
        connection <- UIR.lookup id :: IO Connection
        setPos     connection (model ^. Model.currentFrom . x)  (model ^. Model.currentFrom . y)  (model ^. Model.currentTo . x)  (model ^. Model.currentTo . y)
        setVisible connection (model ^. Model.currentVisible)
        setColor   connection (model ^. Model.currentColor)

instance CompositeWidget Model.Connection
instance CompositeWidget Model.CurrentConnection

