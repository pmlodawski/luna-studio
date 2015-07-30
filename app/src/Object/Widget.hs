module Object.Widget ( T.DisplayObject(..)
                     , T.DisplayObjectClass
                     , T.IsDisplayObject
                     , T.WidgetUpdate
                     , T.WidgetUIUpdate
                     , T.MousePosition
                     , T.MouseButton(..)
                     , T.toMouseButton
                     , onMouseMove
                     , onMousePressed
                     , onMouseReleased
                     , isOver
                     , onMouseOver
                     , onMouseOut
                     , objectId
                     ) where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.CtxDynamic
import qualified Object.Widget.Types as T

onMouseMove, onMouseOver, onMouseOut :: T.MousePosition -> T.DisplayObject -> T.WidgetUpdate
onMouseMove pos = withCtxDynamic (T.onMouseMove pos)
onMouseOver pos = withCtxDynamic (T.onMouseOver pos)
onMouseOut  pos = withCtxDynamic (T.onMouseOut  pos)

isOver :: T.MousePosition -> T.DisplayObject -> Bool
isOver pos = withCtxDynamic (T.isOver pos)

objectId :: T.DisplayObject -> Int
objectId = withCtxDynamic T.objectId

onMousePressed, onMouseReleased :: T.MouseButton -> T.MousePosition -> T.DisplayObject -> T.WidgetUpdate
onMousePressed  button pos = withCtxDynamic (T.onMousePressed  button pos)
onMouseReleased button pos = withCtxDynamic (T.onMouseReleased button pos)
