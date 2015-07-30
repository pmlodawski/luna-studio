module Object.Widget ( T.DisplayObject(..)
                     , T.DisplayObjectClass
                     , T.IsDisplayObject
                     , T.WidgetUpdate
                     , T.WidgetUIUpdate
                     , T.MousePosition
                     , onMouseMove
                     , onMousePress
                     , onMouseRelease
                     , isOver
                     , onMouseOver
                     , onMouseOut
                     , onClick
                     , onDblClick
                     , objectId
                     ) where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.CtxDynamic
import qualified Object.Widget.Types as T
import           Event.Mouse (MouseButton)

onMouseMove, onMouseOver, onMouseOut, onClick, onDblClick :: T.MousePosition -> T.DisplayObject -> T.WidgetUpdate
onMouseMove pos = withCtxDynamic (T.onMouseMove pos)
onMouseOver pos = withCtxDynamic (T.onMouseOver pos)
onMouseOut  pos = withCtxDynamic (T.onMouseOut  pos)
onClick     pos = withCtxDynamic (T.onClick     pos)
onDblClick  pos = withCtxDynamic (T.onDblClick  pos)

onMousePress, onMouseRelease :: MouseButton -> T.MousePosition -> T.DisplayObject -> T.WidgetUpdate
onMousePress   button pos = withCtxDynamic (T.onMousePressed  button pos)
onMouseRelease button pos = withCtxDynamic (T.onMouseReleased button pos)

isOver :: T.MousePosition -> T.DisplayObject -> Bool
isOver pos = withCtxDynamic (T.isOver pos)

objectId :: T.DisplayObject -> Int
objectId = withCtxDynamic T.objectId
