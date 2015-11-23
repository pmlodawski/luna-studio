{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Object.Widget where

import           Utils.PreludePlus hiding ((.=), children)
import           Utils.Vector
import           Utils.CtxDynamic
import           Event.Event    (JSState)
import           Event.Mouse    (MousePosition, MouseButton)
import           Object.UITypes
import qualified Event.Keyboard as Keyboard
import qualified Event.Mouse as Mouse
import           Event.Keyboard (KeyMods)
import           Reactive.State.Camera     (Camera)
import qualified Reactive.State.Camera     as Camera
import           Reactive.Commands.Command (Command)
import           Object.UITypes
import           Data.Aeson (ToJSON, toJSON, object, (.=))
import           Data.HMap.Lazy (HTMap)

type DisplayObject = CtxDynamic DisplayObjectClass

type DisplayObjectCtx a =   ( Show a
                            , Typeable a
                            , IsDisplayObject a
                            , UIDisplayObject a
                            , ToJSON a
                            )

class    DisplayObjectCtx a => DisplayObjectClass a
instance DisplayObjectCtx a => DisplayObjectClass a

-- class DisplayObjectClass a => DisplayObjectContainer a


data WidgetFile b = WidgetFile { _objectId :: WidgetId
                               , _widget   :: b
                               , _parent   :: Maybe WidgetId
                               , _children :: [WidgetId]
                               , _handlers :: HTMap
                               } deriving (Generic)

type Position = Vector2 Double
type Size     = Vector2 Double

class IsDisplayObject a where
    widgetPosition :: Lens'  a Position
    widgetSize     :: Getter a Size

class UIDisplayObject a where
    createUI   :: WidgetId -> WidgetId -> a -> IO ()
    updateUI   :: WidgetId -> a        -> a -> IO ()

getPosition :: DisplayObject -> Position
getPosition obj = withCtxDynamic (^. widgetPosition) obj

setPosition' :: DisplayObjectClass a => Position -> a -> DisplayObject
setPosition' pos obj = toCtxDynamic $ obj & widgetPosition .~ pos

setPosition :: DisplayObject -> Position -> DisplayObject
setPosition obj pos = withCtxDynamic (setPosition' pos) obj

instance IsDisplayObject DisplayObject where
    widgetPosition = lens getPosition setPosition

instance Show DisplayObject where
    show = withCtxDynamic show

instance ToJSON DisplayObject where
    toJSON = withCtxDynamic toJSON

data DragState = DragState { _widgetId       :: WidgetId
                           , _widgetMatrix   :: [Double]
                           , _scene          :: SceneType
                           , _button         :: MouseButton
                           , _keyMods        :: Keyboard.KeyMods
                           , _startPos       :: Vector2 Double
                           , _previousPos    :: Vector2 Double
                           , _currentPos     :: Vector2 Double
                           } deriving (Show, Eq, Generic)

instance ToJSON DragState

sceneToLocal :: Vector2 Double -> [Double] -> Vector2 Double
sceneToLocal (Vector2 x y) [ aa, ab, ac, ad
                           , ba, bb, bc, bd
                           , ca, cb, cc, cd
                           , da, db, dc, dd
                           ] = Vector2 x' y' where
                               x' = aa * x + ba * y + da
                               y' = ab * x + bb * y + db

screenToLocal :: Camera -> Vector2 Int -> [Double]  -> Vector2 Double
screenToLocal cam mousePos widgetMatrix = sceneToLocal workspacePos widgetMatrix where
    workspacePos = Camera.screenToWorkspace cam mousePos

type MouseMoveHandler     s =        Mouse.Event' -> JSState -> WidgetId -> Command s ()
type MousePressedHandler  s =        Mouse.Event' -> JSState -> WidgetId -> Command s ()
type MouseReleasedHandler s =        Mouse.Event' -> JSState -> WidgetId -> Command s ()
type MouseOverHandler     s =                        JSState -> WidgetId -> Command s ()
type MouseOutHandler      s =                        JSState -> WidgetId -> Command s ()
type ClickHandler         s =        Mouse.Event' -> JSState -> WidgetId -> Command s ()
type DblClickHandler      s =        Mouse.Event' -> JSState -> WidgetId -> Command s ()
type KeyUpHandler         s = Char -> KeyMods     -> JSState -> WidgetId -> Command s ()
type KeyDownHandler       s = Char -> KeyMods     -> JSState -> WidgetId -> Command s ()
type KeyPressedHandler    s = Char -> KeyMods     -> JSState -> WidgetId -> Command s ()
type DragMoveHandler      s =         DragState   -> JSState -> WidgetId -> Command s ()
type DragEndHandler       s =         DragState   -> JSState -> WidgetId -> Command s ()

data UIHandlers a  = UIHandlers { _mouseMove     :: MouseMoveHandler      a
                                , _mousePressed  :: MousePressedHandler   a
                                , _mouseReleased :: MouseReleasedHandler  a
                                , _mouseOver     :: MouseOverHandler      a
                                , _mouseOut      :: MouseOutHandler       a
                                , _click         :: ClickHandler          a
                                , _dblClick      :: DblClickHandler       a
                                , _keyUp         :: KeyUpHandler          a
                                , _keyDown       :: KeyDownHandler        a
                                , _keyPressed    :: KeyPressedHandler     a
                                , _dragMove      :: DragMoveHandler       a
                                , _dragEnd       :: DragEndHandler        a
                                }

instance Default (UIHandlers a) where
    def = UIHandlers (\     _ _ _ -> return ())
                     (\     _ _ _ -> return ())
                     (\     _ _ _ -> return ())
                     (\       _ _ -> return ())
                     (\       _ _ -> return ())
                     (\     _ _ _ -> return ())
                     (\     _ _ _ -> return ())
                     (\ _   _ _ _ -> return ())
                     (\ _   _ _ _ -> return ())
                     (\ _   _ _ _ -> return ())
                     (\     _ _ _ -> return ())
                     (\     _ _ _ -> return ())


makeLenses ''DragState

makeLenses ''UIHandlers

makeLenses ''WidgetFile

widgetType :: DisplayObject -> String
widgetType (CtxDynamic tpe _) = show tpe

instance ToJSON (WidgetFile DisplayObject) where
    toJSON file = object [ "_objectId" .= (toJSON $ file ^. objectId)
                         , "_widget"   .= (toJSON $ file ^. widget  )
                         , "_parent"   .= (toJSON $ file ^. parent  )
                         , "_children" .= (toJSON $ file ^. children)
                         , "_type"     .= (toJSON $ widgetType $ file ^. widget)
                         ]
