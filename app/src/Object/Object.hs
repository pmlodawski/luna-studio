module Object.Object where

import Data.Dynamic
import Data.Typeable
import Data.Monoid
import Data.Default
import Control.Lens

import Utils.Wrapper
import Utils.PrettyPrinter

newtype Object a = Object { fromObject :: a } deriving (Eq, Show)

makeLenses ''Object

instance Unwrap Object where unwrap = fromObject

instance PrettyPrinter a => PrettyPrinter (Object a) where
    display (Object o) = "o( " <> display o <> " )"

type ID = Int

data Point = Point { _x :: Int
                   , _y :: Int
                   } deriving (Eq, Show, Typeable)

makeLenses ''Point

instance Default Point where
    def = Point 0 0

instance Num Point where
    (Point x1 y1) + (Point x2 y2) = Point (x1 + x2) (y1 + y2)
    (Point x1 y1) - (Point x2 y2) = Point (x1 - x2) (y1 - y2)
    (Point x1 y1) * (Point x2 y2) = Point (x1 * x2) (y1 * y2)
    abs    (Point x y)            = Point (abs    x) (abs    y)
    signum (Point x y)            = Point (signum x) (signum y)
    fromInteger i                 = let val = fromInteger i in Point val val

instance PrettyPrinter Point where
    display (Point x y) = "(" <> show x <> "," <> show y <> ")"

class Selectable a where
    setSelected       :: a -> Bool -> a
    isSelected        :: a -> Bool
    select            :: a -> a
    unselect          :: a -> a
    toggleSelection   :: a -> a
    select o          = setSelected o True
    unselect o        = setSelected o False
    toggleSelection o = setSelected o $ not . isSelected $ o

instance Selectable a => Selectable (Object a) where
    setSelected (Object o) = Object . setSelected o
    isSelected  (Object o) = isSelected o
