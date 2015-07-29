{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Object.Widget where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.CtxDynamic

type DisplayObject = CtxDynamic DisplayObjectClass

showObject :: DisplayObject -> String
showObject = withCtxDynamic show

type DisplayObjectCtx a = (Show a, MouseMovable a, Typeable a)
class    DisplayObjectCtx a => DisplayObjectClass a
instance DisplayObjectCtx a => DisplayObjectClass a



type WidgetUIUpdate = IO ()
type WidgetUpdate = (WidgetUIUpdate, DisplayObject)

class Pressable a where
    onPress :: a -> WidgetUpdate

class Clickable a where
    onClick :: a -> WidgetUpdate

class MouseMovable a where
    onMouseMove :: a -> WidgetUpdate

class MouseOverable a where
    onMouseOver :: a -> WidgetUpdate

class MouseOutable a where
    onMouseOut :: a -> WidgetUpdate

noUIUpdate :: WidgetUIUpdate
noUIUpdate = return ()

noUpdate :: DisplayObjectClass a => a -> WidgetUpdate
noUpdate w = (noUIUpdate, toCtxDynamic w)

instance {-# OVERLAPPABLE #-} DisplayObjectClass a => MouseMovable a where
    onMouseMove w = (putStrLn "du!!pa", toCtxDynamic w)
