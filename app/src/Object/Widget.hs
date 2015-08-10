{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Object.Widget where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.CtxDynamic
import           Event.Mouse (MouseButton, MousePosition, WidgetId)

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

class HandlesMouseMove     a where onMouseMove     :: MouseButton -> MousePosition -> a -> WidgetUpdate
class HandlesMousePressed  a where onMousePress    :: MouseButton -> MousePosition -> a -> WidgetUpdate
class HandlesMouseReleased a where onMouseRelease  :: MouseButton -> MousePosition -> a -> WidgetUpdate
class HandlesMouseOver     a where onMouseOver     ::                                 a -> WidgetUpdate
class HandlesMouseOut      a where onMouseOut      ::                                 a -> WidgetUpdate
class Clickable            a where onClick         ::                MousePosition -> a -> WidgetUpdate
class DblClickable         a where onDblClick      ::                MousePosition -> a -> WidgetUpdate
class Draggable            a where
    mayDrag         :: MouseButton   -> MousePosition -> a -> Bool
    onDragMove      :: MousePosition -> MousePosition -> a -> WidgetUpdate
    onDragEnd       :: MousePosition -> MousePosition -> a -> WidgetUpdate

instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseMove     a where onMouseMove     _ _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMousePressed  a where onMousePress    _ _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseReleased a where onMouseRelease  _ _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseOver     a where onMouseOver         = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => HandlesMouseOut      a where onMouseOut          = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => Clickable            a where onClick           _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => DblClickable         a where onDblClick        _ = noUpdate
instance {-# OVERLAPPABLE #-} DisplayObjectClass a => Draggable            a where mayDrag       _ _ _ = False
                                                                                   onDragMove      _ _ = noUpdate
                                                                                   onDragEnd       _ _ = noUpdate

instance HandlesMouseMove     DisplayObject where onMouseMove     mb mp (CtxDynamic _ a) = onMouseMove     mb mp a
instance HandlesMousePressed  DisplayObject where onMousePress    mb mp (CtxDynamic _ a) = onMousePress    mb mp a
instance HandlesMouseReleased DisplayObject where onMouseRelease  mb mp (CtxDynamic _ a) = onMouseRelease  mb mp a
instance HandlesMouseOver     DisplayObject where onMouseOver           (CtxDynamic _ a) = onMouseOver           a
instance HandlesMouseOut      DisplayObject where onMouseOut            (CtxDynamic _ a) = onMouseOut            a
instance Clickable            DisplayObject where onClick            mp (CtxDynamic _ a) = onClick            mp a
instance DblClickable         DisplayObject where onDblClick         mp (CtxDynamic _ a) = onDblClick         mp a
instance Draggable            DisplayObject where mayDrag         mb mr (CtxDynamic _ a) = mayDrag         mb mr a
                                                  onDragMove      ma mr (CtxDynamic _ a) = onDragMove      ma mr a
                                                  onDragEnd       ma mr (CtxDynamic _ a) = onDragEnd       ma mr a

noUIUpdate :: WidgetUIUpdate
noUIUpdate = Nothing

noUpdate :: DisplayObjectClass a => a -> WidgetUpdate
noUpdate w = (noUIUpdate, toCtxDynamic w)

