module Utils.Vector where

import           Utils.PreludePlus


data Vector2 a = Vector2 { _x :: a
                         , _y :: a
                         } deriving (Eq, Show, Functor)

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

lengthSquared :: Num a => Vector2 a -> a
lengthSquared (Vector2 x y) = x * x + y * y

normalize :: Vector2 Double -> Vector2 Double
normalize (Vector2 x y) = Vector2 (x / len) (y / len) where len = sqrt $ x*x + y*y


instance Applicative Vector2 where
    pure v                          = Vector2 v v
    (Vector2 f g) <*> (Vector2 x y) = Vector2 (f x) (g y)

instance Monoid a => Monoid (Vector2 a) where
    mempty                                    = Vector2 mempty mempty
    (Vector2 x1 y1) `mappend` (Vector2 x2 y2) = Vector2 (x1 `mappend` x2) (y1 `mappend` y2)

negateSnd :: Num a => Vector2 a -> Vector2 a
negateSnd (Vector2 x y) = Vector2 x (-y)

instance PrettyPrinter a => PrettyPrinter (Vector2 a) where
    display (Vector2 x y) = "(" <> display x <> "," <> display y <> ")"
