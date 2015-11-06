{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global as Global

import           UI.Widget (UIWidget(..))
import qualified UI.Widget as Widget
import qualified UI.Registry as UIR
import qualified UI.Generic  as UI
import           Reactive.Commands.Command (Command, ioCommand, performIO)


newtype Slider = Slider { unSlider :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget Slider

foreign import javascript unsafe "new Slider($1, $2, $3)"   createSlider'     :: Int    -> Double -> Double -> IO Slider
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
    UI.setWidgetPosition (model ^. widgetPosition) slider
    return slider

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

instance Model.IsSlider a => UIDisplayObject (Model.Slider a) where
    createUI parentId id model = do
        slider   <- createSlider id model
        parent <- UIR.lookup parentId :: IO Widget.GenericWidget
        UIR.register id slider
        Widget.add slider parent

    updateUI id old model = do
        slider <- UIR.lookup id :: IO Slider

        setLabel       model slider
        setValueLabel  model slider
        setValue       model slider

--                           action = setCursor "pointer"
--     onDragMove  state file model = ifEnabled model (action, toCtxDynamic newModel) where
--                     delta        = if (abs $ diff ^. x) > (abs $ diff ^. y) then -diff ^. x /  divider
--                                                                             else  diff ^. y / (divider * 10.0)
--                     width        = model ^. Model.size . x
--                     divider      = width * (keyModMult $ state ^. keyMods)
--                     diff         = state ^. currentPos - state ^. previousPos
--                     newNormValue = (model ^. Model.normValue) - delta
--                     newModel     = Model.setNormValue newNormValue model
--                     action       = do
--                         setCursor "-webkit-grabbing"
--                         slider  <- UIR.lookup (file ^. objectId)
--                         setValue newModel slider

dblClickHandler :: DblClickHandler Global.State
dblClickHandler evt id = zoom Global.uiRegistry $ do
    enabled <- UICmd.get id (Model.enabled :: Lens' (Model.Slider Double) Bool)
    when enabled $ do
        width <- UICmd.get id $ (Model.size :: Lens' (Model.Slider Double) (Vector2 Double)) . x
        let normValue = (evt ^. Mouse.position ^. x) / width
        UICmd.update id ((Model.boundedNormValue :: Lens' (Model.Slider Double) Double) .~ normValue)


keyUpHandler :: KeyUpHandler Global.State
keyUpHandler 'W' _ id = zoom Global.uiRegistry $ do
    enabled <- UICmd.get id (Model.enabled :: Lens' (Model.Slider Double) Bool)
    when enabled $ UICmd.update id ((Model.boundedNormValue :: Lens' (Model.Slider Double) Double) +~ 0.1)

keyUpHandler 'Q' _ id = zoom Global.uiRegistry $ do
    enabled <- UICmd.get id (Model.enabled :: Lens' (Model.Slider Double) Bool)
    when enabled $ UICmd.update id ((Model.boundedNormValue :: Lens' (Model.Slider Double) Double) -~ 0.1)

keyUpHandler _ _ _ = return ()

dragHandler :: DragMoveHandler Global.State
dragHandler ds id = zoom Global.uiRegistry $ do
    enabled <- UICmd.get id (Model.enabled :: Lens' (Model.Slider Double) Bool)
    when enabled $ do
        width <- UICmd.get id $ (Model.size :: Lens' (Model.Slider Double) (Vector2 Double)) . x
        let normValue = (ds ^. currentPos . x) / width
        UICmd.update id ((Model.boundedNormValue :: Lens' (Model.Slider Double) Double) .~ normValue)

dragEndHandler _ _ = performIO $ putStrLn "Trigger slider event ValueChanged"

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyUp    .~ keyUpHandler
                     & dblClick .~ dblClickHandler
                     & mousePressed .~ UI.startDrag
                     & dragMove .~ dragHandler
                     & dragEnd .~ dragEndHandler
