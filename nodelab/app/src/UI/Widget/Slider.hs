{-# LANGUAGE OverloadedStrings #-}

module UI.Widget.Slider where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           GHCJS.Types      ( JSVal, JSString )
import           Data.JSString.Text ( lazyTextFromJSString, lazyTextToJSString )
import qualified Data.JSString as JSString

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import qualified Event.Mouse    as Mouse
import           Event.Keyboard (KeyMods(..))
import           Utils.Vector
import qualified Object.Widget.Slider as Model
import           Object.Widget
import           Utils.CtxDynamic
import           JS.UI (setCursor)
import           Object.UITypes
import           GHCJS.Marshal.Pure(PToJSVal(..), PFromJSVal(..))

import           UI.Types (UIWidget(..))
import qualified UI.Registry as UIR

newtype Slider = Slider { unSlider :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget Slider

foreign import javascript unsafe "new Slider($1, $2, $3)"   createSlider'     :: Int    -> Double -> Double -> IO Slider
foreign import javascript unsafe "$1.mesh.position.x = $2; $1.mesh.position.y = $3"
                                                            setPosition'      :: Slider -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.setValue($2)"          setValue'         :: Slider -> Double           -> IO ()
foreign import javascript unsafe "$1.setLabel($2)"          setLabel'         :: Slider -> JSString         -> IO ()
foreign import javascript unsafe "$1.setValueLabel($2)"     setValueLabel'    :: Slider -> JSString         -> IO ()
foreign import javascript unsafe "$1.setFocus($2)"          setFocus'         :: Slider -> Bool             -> IO ()

createSlider :: (Model.IsSlider a) => WidgetId -> Model.Slider a -> IO Slider
createSlider oid model = do
    slider      <- createSlider' oid (model ^. Model.size . x) (model ^. Model.size . y)
    setLabel       model slider
    setValueLabel  model slider
    setValue       model slider
    setPosition    model slider
    return slider

setPosition :: (Model.IsSlider a) => Model.Slider a -> Slider -> IO ()
setPosition model slider = setPosition' slider (model ^. Model.pos . x) (model ^. Model.pos . y)

setValueLabel :: (Model.IsSlider a) => Model.Slider a -> Slider -> IO ()
setValueLabel model slider = setValueLabel' slider $ JSString.pack $ Model.displayValue model

setLabel :: Model.Slider a -> Slider -> IO ()
setLabel model slider = setLabel' slider $ lazyTextToJSString $ model ^. Model.label

setFocus :: Bool -> Slider -> IO ()
setFocus = flip setFocus'

setValue :: Model.Slider a -> Slider -> IO ()
setValue model slider = setValue' slider $ model ^. Model.normValue

keyModMult :: KeyMods -> Double
keyModMult mods = case mods of
    KeyMods True  True  _ _ -> 1000.0
    KeyMods False True  _ _ ->  100.0
    KeyMods True  False _ _ ->   10.0
    otherwise               ->    1.0

ifEnabled :: (Model.IsSlider a) => Model.Slider a -> WidgetUpdate -> WidgetUpdate
ifEnabled model upd = if model ^. Model.enabled then upd
                                                else (return(), toCtxDynamic model)

instance (Model.IsSlider a) => Draggable (Model.Slider a) where
    mayDrag Mouse.LeftButton _ _ _     = True
    mayDrag _                _ _ _     = False
    onDragStart state file model = ifEnabled model (action, toCtxDynamic model) where
                          action = setCursor "pointer"
    onDragMove  state file model = ifEnabled model (action, toCtxDynamic newModel) where
                    delta        = if (abs $ diff ^. x) > (abs $ diff ^. y) then -diff ^. x /  divider
                                                                            else  diff ^. y / (divider * 10.0)
                    width        = model ^. Model.size . x
                    divider      = width * (keyModMult $ state ^. keyMods)
                    diff         = state ^. currentPos - state ^. previousPos
                    newNormValue = (model ^. Model.normValue) - delta
                    newModel     = Model.setNormValue newNormValue model
                    action       = do
                        setCursor "-webkit-grabbing"
                        slider  <- UIR.lookup (file ^. objectId)
                        setValue newModel slider
    onDragEnd  state file model  = ifEnabled model (action, newModel) where
        action = do
            otherAction
            setCursor "default"
        (otherAction, newModel) = onDragMove state file model

instance (Model.IsSlider a)  => DblClickable   (Model.Slider a) where
    onDblClick pos file model = ifEnabled model (action, toCtxDynamic newModel) where
                normValue     = (pos ^. x) / (model ^. Model.size . x)
                newModel      = Model.setNormValue normValue model
                action        = UIR.lookup (file ^. objectId) >>= setValue newModel

instance (Model.IsSlider a) => HandlesMouseOver (Model.Slider a) where
    onMouseOver file model   = ifEnabled model (action, toCtxDynamic model) where
                 action      = UIR.lookup (file ^. objectId) >>= setFocus True

instance (Model.IsSlider a) => HandlesMouseOut (Model.Slider a) where
    onMouseOut  file model   = ifEnabled model (action, toCtxDynamic model) where
                 action      = UIR.lookup (file ^. objectId) >>= setFocus False

instance (Model.IsSlider a) => Focusable (Model.Slider a) where
    mayFocus _ _ _ _  = True

instance (Model.IsSlider a) => HandlesKeyUp (Model.Slider a) where
    onKeyUp 'W' _ file model   = ifEnabled model (action, toCtxDynamic newModel) where
                  currVal      = model ^. Model.normValue
                  newModel     = Model.setNormValue (currVal + 0.1) model
                  action       = UIR.lookup (file ^. objectId) >>= setValue newModel
    onKeyUp 'Q' _ file model   = ifEnabled model (action, toCtxDynamic newModel) where
                  currVal      = model ^. Model.normValue
                  newModel     = Model.setNormValue (currVal - 0.1) model
                  action       = UIR.lookup (file ^. objectId) >>= setValue newModel
    onKeyUp _   _ _    model   = (return (), toCtxDynamic model)
