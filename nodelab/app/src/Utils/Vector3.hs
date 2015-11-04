module Utils.Vector3 where

import           Utils.PreludePlus


data Vector3 a = Vector3 { _x :: a
                         , _y :: a
                         } deriving (Eq, Show, Functor)

makeLenses ''Vector3

instance Default a => Default (Vector3 a) where
    def = Vector3 def def def

instance Num a => Num (Vector3 a) where
    (Vector3 x1 y1 z1) + (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)
    (Vector3 x1 y1 z1) - (Vector3 x2 y2 z2) = Vector3 (x1 - x2) (y1 - y2) (z1 - z2)
    (Vector3 x1 y1 z1) * (Vector3 x2 y2 z2) = Vector3 (x1 * x2) (y1 * y2) (z1 * z2)
    abs    (Vector3 x y z)                  = Vector3 (abs    x) (abs    y)
    signum (Vector3 x y z)                  = Vector3 (signum x) (signum y)
    fromInteger i                           = pure $ fromInteger i

instance Applicative Vector3 where
    pure v                              = Vector3 v v v
    (Vector3 f g h) <*> (Vector3 x y z) = Vector3 (f x) (g y) (h z)

instance Monoid a => Monoid (Vector3 a) where
    mempty                                          = Vector3 mempty mempty mempty
    (Vector3 x1 y1 z1) `mappend` (Vector3 x2 y2 z2) = Vector3 (x1 `mappend` x2) (y1 `mappend` y2) (z1 `mappend` z2)

instance PrettyPrinter a => PrettyPrinter (Vector3 a) where
    display (Vector3 x y z) = "(" <> display x <> "," <> display y <> "," <> display z <> ")"
