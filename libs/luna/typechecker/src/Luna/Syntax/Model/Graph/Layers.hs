{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Graph.Layers where

import Prologue hiding (read)

import           Control.Monad.Event
import           Data.Attribute
import           Data.Construction
import qualified Luna.Syntax.Model.Builder.Type as Type
import qualified Luna.Syntax.Model.Builder.Self as Self
import           Luna.Syntax.Model.Builder.Self (MonadSelfBuilder, self)
import           Luna.Syntax.Model.Graph.Class
import           Luna.Syntax.Model.Layer


--------------------------------
-- === Succs registration === --
--------------------------------

-- === Definitions === ---

data SuccRegister = SuccRegister deriving (Show)
instance ( Monad  m
         , Reader m (Edge src tgt)
         , Reader m (Node src)
         , Writer m (Node src)
         , Show src
         , Attr Succs src ~ [Ref (Edge src tgt)]
         , HasAttr Succs src
         ) => Handler t SuccRegister m (Ref (Edge src tgt)) where 
    handler e = do
        ve <- lift $ read e -- FIXME[WD]: remove the lift (it could be handy to disable the magic trans-instance in Graph.hs)
        lift $ with (ve ^. source) $ attr Succs %~ (e:)
    {-# INLINE handler #-}

instance Monad m => Destroyer m (Layer (Network ls) Succs a) where
    destroy _ = return ()

-- === Utils === ---

registerSuccs :: t -> Listener t SuccRegister m a -> m a
registerSuccs _ = unwrap'



------------------------------------------
-- === Native layers implementation === --
------------------------------------------

-- === Succs layer === --

type instance LayerData (Network ls) Succs t = [Ref $ Link (Shelled t)]
instance Monad m => Maker m (Layer (Network ls) Succs a) where make = return $ Layer []


-- === Type layer === --

type instance LayerData (Network ls) Type t = Ref $ Link (Shelled t)

instance (MonadSelfBuilder s m, Ref (Link l) ~ Connection s (Ref $ Node l), Connectible s (Ref $ Node l) m, l ~ Shelled a)
      => Maker m (Layer (Network ls) Type a) where 
    make = Layer <$> do
        s <- self
        let tgt = Ref $ Ptr 0 :: Ref $ Node l -- FIXME[WD]: Pure magic. 0 is the ID of Star
        connection tgt s

instance (Monad m, Unregister m (LayerData (Network ls) Type a)) => Destroyer m (Layer (Network ls) Type a) where
    destroy (Layer ref) = unregister ref
