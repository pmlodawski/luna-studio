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

data Vector2 a = Vector2 { _x :: a
                         , _y :: a
                         } deriving (Eq, Show)

makeLenses ''Vector2

instance Default a => Default (Vector2 a) where
    def = Vector2 def def

instance Num a => Num (Vector2 a) where
    (Vector2 x1 y1) + (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)
    (Vector2 x1 y1) - (Vector2 x2 y2) = Vector2 (x1 - x2) (y1 - y2)
    (Vector2 x1 y1) * (Vector2 x2 y2) = Vector2 (x1 * x2) (y1 * y2)
    abs    (Vector2 x y)              = Vector2 (abs    x) (abs    y)
    signum (Vector2 x y)              = Vector2 (signum x) (signum y)
    fromInteger i                     = let val = fromInteger i in Vector2 val val

instance Functor Vector2 where
    fmap f (Vector2 x y) = Vector2 (f x) (f y)

vector2FromIntegral :: (Integral a, Num b) => Vector2 a -> Vector2 b
vector2FromIntegral (Vector2 x y) = Vector2 (fromInteger . toInteger $ x) (fromInteger . toInteger $ y)


instance PrettyPrinter a => PrettyPrinter (Vector2 a) where
    display (Vector2 x y) = "(" <> display x <> "," <> display y <> ")"




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
