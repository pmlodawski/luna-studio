module Object.Widget ( T.DisplayObject(..)
                     , T.DisplayObjectClass
                     , T.HasObjectId
                     , T.objectId
                     , T.WidgetUpdate
                     , T.WidgetUIUpdate
                     , onMouseMove
                     ) where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.CtxDynamic
import qualified Object.Widget.Types as T

onMouseMove :: Vector2 Int -> T.DisplayObject -> T.WidgetUpdate
onMouseMove pos = withCtxDynamic (T.onMouseMove pos)
