{-# LANGUAGE OverloadedStrings #-}

module UI.Widget.Slider where

import           Utils.PreludePlus


import           GHCJS.Foreign
import           GHCJS.Types      ( JSRef, JSString )
import           GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import           Data.JSString.Text ( lazyTextFromJSString, lazyTextToJSString )
import qualified Data.JSString as JSString

import qualified JavaScript.Object as JSObject

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import qualified Event.Mouse    as Mouse
import           Event.Keyboard (KeyMods(..))
import           Utils.Vector
import qualified Object.Widget.Slider as Model
import           Object.Widget
import           GHCJS.Prim
import           Utils.CtxDynamic
import           JS.Bindings (setCursor)
import           Object.UITypes

newtype Slider = Slider { unSlider :: JSVal }

foreign import javascript unsafe "new Slider($1, $2, $3)"   createSlider'     :: Int    -> Double      -> Double  -> IO Slider
foreign import javascript unsafe "common.registry[$1] = $2" registerSlider'   :: Int    -> Slider           -> IO ()
foreign import javascript unsafe "common.registry[$1]"      getSlider         :: Int                        -> IO Slider
foreign import javascript unsafe "$1.mesh.position.x = $2; $1.mesh.position.y = $3"
                                                            setPosition'      :: Slider -> Double -> Double -> IO ()
foreign import javascript unsafe "$1.setValue($2)"          setValue'         :: Slider -> Double           -> IO ()
foreign import javascript unsafe "$1.setLabel($2)"          setLabel'         :: Slider -> JSString         -> IO ()
foreign import javascript unsafe "$1.setValueLabel($2)"     setValueLabel'    :: Slider -> JSString         -> IO ()
foreign import javascript unsafe "$1.setFocus($2)"          setFocus'         :: Slider -> Bool             -> IO ()

foreign import javascript unsafe "common.registry[$1].expandedNode.add($2.mesh)" addSlider :: WidgetId -> Slider -> IO ()

createSlider :: (Model.IsSlider a) => WidgetId -> Model.Slider a -> IO Slider
createSlider oid model = do
    slider          <- createSlider' oid (model ^. Model.size . x) (model ^. Model.size . y)
    setLabel        slider model
    setValueLabel   slider model
    setValue        slider model
    setPosition     slider model
    registerSlider' oid slider
    return slider

setPosition :: (Model.IsSlider a) => Slider -> Model.Slider a -> IO ()
setPosition slider model = setPosition' slider (model ^. Model.pos . x) (model ^. Model.pos . y)

setValueLabel :: (Model.IsSlider a) => Slider -> Model.Slider a -> IO ()
setValueLabel slider model = do
    let text = Model.displayValue model
    setValueLabel' slider (JSString.pack text)

setLabel :: Slider -> Model.Slider a -> IO ()
setLabel slider model = setLabel' slider $ lazyTextToJSString $ model ^. Model.label

setFocus :: Slider -> Bool -> IO ()
setFocus = setFocus'

setValue :: Slider -> Model.Slider a -> IO ()
setValue slider model = setValue' slider $ model ^. Model.normValue

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
                        slider <- getSlider (file ^. objectId)
                        setValue slider newModel
    onDragEnd  state file model  = ifEnabled model (action, newModel) where
        action = do
            otherAction
            setCursor "default"
        (otherAction, newModel) = onDragMove state file model

instance (Model.IsSlider a) => DblClickable   (Model.Slider a) where
    onDblClick pos file model = ifEnabled model (action, toCtxDynamic newModel) where
                normValue     = (pos ^. x) / (model ^. Model.size . x)
                newModel      = Model.setNormValue normValue model
                action        = do
                    slider <- getSlider (file ^. objectId)
                    setValue slider newModel

instance (Model.IsSlider a) => HandlesMouseOver (Model.Slider a) where
    onMouseOver file model = ifEnabled model (action, toCtxDynamic model) where
                 action    = do
                     slider <- getSlider (file ^. objectId)
                     setFocus slider True

instance (Model.IsSlider a) => HandlesMouseOut (Model.Slider a) where
    onMouseOut  file model = ifEnabled model (action, toCtxDynamic model) where
                 action    = do
                     slider <- getSlider (file ^. objectId)
                     setFocus slider False

instance (Model.IsSlider a) => Focusable (Model.Slider a) where
    mayFocus _ _ _ _  = True

instance (Model.IsSlider a) => HandlesKeyUp (Model.Slider a) where
    onKeyUp 'W' _ file model   = ifEnabled model (action, toCtxDynamic newModel) where
                  currVal      = model ^. Model.normValue
                  newModel     = Model.setNormValue (currVal + 0.1) model
                  action       = do
                      slider <- getSlider (file ^. objectId)
                      setValue slider newModel
    onKeyUp 'Q' _ file model   = ifEnabled model (action, toCtxDynamic newModel) where
                  currVal      = model ^. Model.normValue
                  newModel     = Model.setNormValue (currVal - 0.1) model
                  action       = do
                      slider <- getSlider (file ^. objectId)
                      setValue slider newModel
    onKeyUp _   _ _    model   = (return (), toCtxDynamic model)
