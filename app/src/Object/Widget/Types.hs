{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Object.Widget.Types where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.CtxDynamic
import           Event.Mouse (MouseButton)

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
                            )

class    DisplayObjectCtx a => DisplayObjectClass a
instance DisplayObjectCtx a => DisplayObjectClass a

type WidgetUIUpdate = Maybe (IO ())
type WidgetUpdate = (WidgetUIUpdate, DisplayObject)

class IsDisplayObject a where
    objectId :: a -> Int
    isOver   :: MousePosition -> a -> Bool

type MousePosition = Vector2 Int

class HandlesMouseMove     a where onMouseMove     ::                MousePosition -> a -> WidgetUpdate
class HandlesMousePressed  a where onMousePressed  :: MouseButton -> MousePosition -> a -> WidgetUpdate
class HandlesMouseReleased a where onMouseReleased :: MouseButton -> MousePosition -> a -> WidgetUpdate
class HandlesMouseOver     a where onMouseOver     ::                MousePosition -> a -> WidgetUpdate
class HandlesMouseOut      a where onMouseOut      ::                MousePosition -> a -> WidgetUpdate
class Clickable            a where onClick         ::                MousePosition -> a -> WidgetUpdate
class DblClickable         a where onDblClick      ::                MousePosition -> a -> WidgetUpdate

instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseMove     a where onMouseMove       _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMousePressed  a where onMousePressed  _ _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseReleased a where onMouseReleased _ _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseOver     a where onMouseOver       _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseOut      a where onMouseOut        _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => Clickable            a where onClick           _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => DblClickable         a where onDblClick        _ = noUpdate

noUIUpdate :: WidgetUIUpdate
noUIUpdate = Nothing

noUpdate :: DisplayObjectClass a => a -> WidgetUpdate
noUpdate w = (noUIUpdate, toCtxDynamic w)

