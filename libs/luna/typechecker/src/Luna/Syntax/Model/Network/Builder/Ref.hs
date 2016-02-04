module Luna.Syntax.Model.Network.Builder.Ref where

import Prelude.Luna

import Luna.Syntax.Model.Network.Graph.Class


type RefHandler m a = (Reader m a, Writer m a)
class Monad m => Reader m a where read  :: Ref a -> m a
class Monad m => Writer m a where write :: Ref a -> a -> m ()

withM :: RefHandler m a => Ref a -> (a -> m a) -> m ()
withM ref f = read ref >>= f >>= write ref

with :: RefHandler m a => Ref a -> (a -> a) -> m ()
with ref = withM ref ∘ (return <$>)