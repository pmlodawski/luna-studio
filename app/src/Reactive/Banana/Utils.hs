module Reactive.Banana.Utils where

import Control.Applicative ( Applicative, liftA2 )
import Data.Monoid         ( Monoid, (<>) )
import Reactive.Banana     ( Event, (<$), filterE )

(<<*>>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<<*>>) = liftA2 (<>)

filterE_ :: (a -> Bool) -> Event t a -> Event t ()
filterE_ p event = () <$ filterE p event
