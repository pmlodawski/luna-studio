{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Object.Widget where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.CtxDynamic
import           Event.Mouse    (MouseButton, MousePosition, WidgetId)
import           Event.Keyboard (KeyMods)
import           Reactive.Plugins.Core.Action.State.Camera (Camera)
import qualified Reactive.Plugins.Core.Action.State.Camera as Camera
import qualified JS.Camera as JSCamera
import           ThreeJS.Types

type DisplayObject = CtxDynamic DisplayObjectClass

showObject :: DisplayObject -> String
showObject = withCtxDynamic show

type DisplayObjectCtx a =   ( Show a
                            , Typeable a
                            , IsDisplayObject a
                            , HandlesMouseMove a
                            , HandlesMousePressed a
                            , HandlesMouseReleased a
                            , HandlesMouseOver a
                            , HandlesMouseOut a
                            , Clickable a
                            , DblClickable a
                            , Draggable a
                            )

class    DisplayObjectCtx a => DisplayObjectClass a
instance DisplayObjectCtx a => DisplayObjectClass a

type WidgetUIUpdate = Maybe (IO ())
type WidgetUpdate   = (WidgetUIUpdate, DisplayObject)

class IsDisplayObject a where objectId :: a -> WidgetId

instance IsDisplayObject DisplayObject where objectId (CtxDynamic _ a) = objectId a

type Position = Vector2 Double

class HandlesMouseMove     a where onMouseMove     :: MouseButton -> Position -> a -> WidgetUpdate
class HandlesMousePressed  a where onMousePress    :: MouseButton -> Position -> a -> WidgetUpdate
class HandlesMouseReleased a where onMouseRelease  :: MouseButton -> Position -> a -> WidgetUpdate
class HandlesMouseOver     a where onMouseOver     ::                            a -> WidgetUpdate
class HandlesMouseOut      a where onMouseOut      ::                            a -> WidgetUpdate
class Clickable            a where onClick         ::                Position -> a -> WidgetUpdate
class DblClickable         a where onDblClick      ::                Position -> a -> WidgetUpdate
class Draggable            a where
    mayDrag         :: MouseButton -> Position -> a -> Bool
    onDragStart     ::               DragState -> a -> WidgetUpdate
    onDragMove      ::               DragState -> a -> WidgetUpdate
    onDragEnd       ::               DragState -> a -> WidgetUpdate

instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseMove     a where onMouseMove     _ _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMousePressed  a where onMousePress    _ _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseReleased a where onMouseRelease  _ _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseOver     a where onMouseOver         = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseOut      a where onMouseOut          = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => Clickable            a where onClick           _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => DblClickable         a where onDblClick        _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => Draggable            a where mayDrag       _ _ _ = False
                                                                                   onDragStart       _ = noUpdate
                                                                                   onDragMove        _ = noUpdate
                                                                                   onDragEnd         _ = noUpdate

instance HandlesMouseMove     DisplayObject where onMouseMove     mb mp    (CtxDynamic _ a) = onMouseMove     mb mp    a
instance HandlesMousePressed  DisplayObject where onMousePress    mb mp    (CtxDynamic _ a) = onMousePress    mb mp    a
instance HandlesMouseReleased DisplayObject where onMouseRelease  mb mp    (CtxDynamic _ a) = onMouseRelease  mb mp    a
instance HandlesMouseOver     DisplayObject where onMouseOver              (CtxDynamic _ a) = onMouseOver              a
instance HandlesMouseOut      DisplayObject where onMouseOut               (CtxDynamic _ a) = onMouseOut               a
instance Clickable            DisplayObject where onClick            mp    (CtxDynamic _ a) = onClick            mp    a
instance DblClickable         DisplayObject where onDblClick         mp    (CtxDynamic _ a) = onDblClick         mp    a
instance Draggable            DisplayObject where mayDrag         mb mr    (CtxDynamic _ a) = mayDrag         mb mr    a
                                                  onDragStart           ds (CtxDynamic _ a) = onDragStart           ds a
                                                  onDragMove            ds (CtxDynamic _ a) = onDragMove            ds a
                                                  onDragEnd             ds (CtxDynamic _ a) = onDragEnd             ds a

noUIUpdate :: WidgetUIUpdate
noUIUpdate = Nothing

noUpdate :: DisplayObjectClass a => a -> WidgetUpdate
noUpdate w = (noUIUpdate, toCtxDynamic w)

data DragState = DragState { _widgetId       :: WidgetId
                           , _widgetMatrix   :: [Double]
                           , _scene          :: SceneType
                           , _button         :: MouseButton
                           , _keyMods        :: KeyMods
                           , _startPos       :: Vector2 Double
                           , _previousPos    :: Vector2 Double
                           , _currentPos     :: Vector2 Double
                           } deriving (Show, Eq)
makeLenses ''DragState

sceneToLocal :: Vector2 Double -> [Double] -> Vector2 Double
sceneToLocal (Vector2 x y) [ aa, ab, ac, ad
                           , ba, bb, bc, bd
                           , ca, cb, cc, cd
                           , da, db, dc, dd
                           ] = Vector2 x' y' where
                               x' = aa * x + ba * y + da
                               y' = ab * x + bb * y + db

screenToLocal :: JSCamera.Camera -> Vector2 Int -> [Double]  -> Vector2 Double
screenToLocal cam mousePos widgetMatrix = sceneToLocal workspacePos widgetMatrix where
    workspacePos = JSCamera.screenToWorkspace cam mousePos
