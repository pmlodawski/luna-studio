{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Network.Builder.Layer where

import Prologue hiding (read)

import           Control.Monad.Event
import           Data.Attribute
import           Data.Construction
import qualified Luna.Syntax.Model.Network.Builder.Type as Type
import qualified Luna.Syntax.Model.Network.Builder.Self as Self
import           Luna.Syntax.Model.Network.Builder.Self (MonadSelfBuilder, self)
import           Luna.Syntax.Model.Graph
import           Luna.Syntax.Model.Graph.Builder.Class
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Graph.Builder.Ref
import qualified Luna.Syntax.Model.Graph.Builder.Ref as Ref
import           Luna.Syntax.Model.Network.Class


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
        lift $ Ref.with (ve ^. source) $ attr Succs %~ (e:)
    {-# INLINE handler #-}

instance Monad m => Destructor m (Layer (Network ls) Succs a) where
    destruct _ = return ()

-- === Utils === ---

registerSuccs :: t -> Listener t SuccRegister m a -> m a
registerSuccs _ = unwrap'



------------------------------------------
-- === Native layers implementation === --
------------------------------------------

-- === Succs layer === --

type instance LayerData (Network ls) Succs t = [Ref $ Link (Shelled t)]
instance Monad m => Creator m (Layer (Network ls) Succs a) where create = return $ Layer []


-- === Type layer === --

type instance LayerData (Network ls) Type t = Ref $ Link (Shelled t)

instance (MonadSelfBuilder s m, Ref (Link l) ~ Connection s (Ref $ Node l), Connectible s (Ref $ Node l) m, l ~ Shelled a)
      => Creator m (Layer (Network ls) Type a) where 
    create = Layer <$> do
        s <- self
        let tgt = Ref $ Ptr 0 :: Ref $ Node l -- FIXME[WD]: Pure magic. 0 is the ID of Star
        connection tgt s

instance (Monad m, Unregister m (LayerData (Network ls) Type a)) => Destructor m (Layer (Network ls) Type a) where
    destruct (Layer ref) = unregister ref
