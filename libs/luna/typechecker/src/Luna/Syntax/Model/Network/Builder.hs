{-# LANGUAGE CPP                       #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE RankNTypes                #-}


module Luna.Syntax.Model.Network.Builder (module Luna.Syntax.Model.Network.Builder, module X) where


import Prologue hiding (Getter, Setter, read, (#))

import Luna.Syntax.Model.Graph.Builder.Class as X
import Luna.Syntax.Model.Network.Builder.Term  as X

import Data.Prop
import Control.Monad.Catch            (MonadMask, MonadCatch, MonadThrow)
import Data.Construction
import Data.Container
import Data.Index
import Luna.Syntax.Model.Graph
import Data.Layer.Cover
import Luna.Syntax.Model.Graph.Builder.Ref




-- === Utils === --


follow :: (Reader m (Edge src tgt), Functor m) => Ref (Edge src tgt) -> m (Ref $ Node tgt)
follow ptr = view target <$> read ptr

reconnect :: (Reader m n1, Writer m n1, Connectible (Ref n1) (Ref n2) m, e ~ Connection (Ref n1) (Ref n2), Unregister m e)
          => Ref n1 -> Lens' n1 e -> Ref n2 -> m e
reconnect srcRef l tgtRef = do
    src  <- read srcRef
    unregister $ src ^. l
    conn <- connection srcRef tgtRef
    write srcRef $ src & l .~ conn
    return conn






