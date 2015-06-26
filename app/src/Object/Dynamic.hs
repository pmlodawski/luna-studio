module Object.Dynamic where

import Data.Dynamic
import Data.Maybe    ( catMaybes )

import Utils.Wrapper
import Object.Object

class UnpackDynamic a b where
    unpackDynamic :: a -> b

instance Typeable a => UnpackDynamic (Object Dynamic) (Maybe a) where
    unpackDynamic = fromDynamic . unwrap

instance Typeable a => UnpackDynamic [Object Dynamic] [Object a] where
    unpackDynamic = fmap Object . catMaybes . fmap unpackDynamic
