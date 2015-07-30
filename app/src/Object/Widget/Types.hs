{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Object.Widget.Types where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.CtxDynamic

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
                            )

class    DisplayObjectCtx a => DisplayObjectClass a
instance DisplayObjectCtx a => DisplayObjectClass a

type WidgetUIUpdate = Maybe (IO ())
type WidgetUpdate = (WidgetUIUpdate, DisplayObject)

class IsDisplayObject a where
    objectId :: a -> Int
    isOver   :: MousePosition -> a -> Bool

-- class Pressable a where
--     onPress :: a -> WidgetUpdate
--
-- class Clickable a where
--     onClick :: a -> WidgetUpdate

type MousePosition = Vector2 Int
data MouseButton =  NoButton | LeftMouseButton | CenterMouseButton | RightMouseButton

class HandlesMouseMove     a where onMouseMove     ::                MousePosition -> a -> WidgetUpdate
class HandlesMousePressed  a where onMousePressed  :: MouseButton -> MousePosition -> a -> WidgetUpdate
class HandlesMouseReleased a where onMouseReleased :: MouseButton -> MousePosition -> a -> WidgetUpdate
class HandlesMouseOver     a where onMouseOver     ::                MousePosition -> a -> WidgetUpdate
class HandlesMouseOut      a where onMouseOut      ::                MousePosition -> a -> WidgetUpdate

instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseMove     a where onMouseMove       _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMousePressed  a where onMousePressed  _ _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseReleased a where onMouseReleased _ _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseOver     a where onMouseOver       _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseOut      a where onMouseOut        _ = noUpdate

-- class MouseOverable a where
--     onMouseOver :: a -> WidgetUpdate
--
-- class MouseOutable a where
--     onMouseOut :: a -> WidgetUpdate

noUIUpdate :: WidgetUIUpdate
noUIUpdate = Nothing

noUpdate :: DisplayObjectClass a => a -> WidgetUpdate
noUpdate w = (noUIUpdate, toCtxDynamic w)




toMouseButton :: Int -> MouseButton
toMouseButton -1 = NoButton
toMouseButton  0 = LeftMouseButton
toMouseButton  1 = CenterMouseButton
toMouseButton  2 = RightMouseButton
toMouseButton  _ = NoButton
