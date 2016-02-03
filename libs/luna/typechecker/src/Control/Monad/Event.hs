{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Event where


import Prologue

import Control.Monad.Fix
import Control.Monad.State (StateT)

----------------------
-- === Listener === --
----------------------

-- === Definitions === --

class Handler    t cfg m a where handler :: a -> Listener t cfg m ()
newtype Listener t cfg m a = Listener (m a) deriving (Show)
makeWrapped ''Listener


-- === Instances === --

instance Functor  m    => Functor     (Listener t cfg m) where fmap     f = wrapped %~ fmap f                    ; {-# INLINE fmap   #-}
instance Monad    m    => Monad       (Listener t cfg m) where (>>=) tc f = wrap' $ unwrap' tc >>= unwrap' <$> f ; {-# INLINE (>>=)  #-}
instance MonadIO  m    => MonadIO     (Listener t cfg m) where liftIO     = wrap' ∘ liftIO                       ; {-# INLINE liftIO #-}
instance MonadFix m    => MonadFix    (Listener t cfg m) where mfix     f = wrap' $ mfix $ unwrap' <$> f         ; {-# INLINE mfix   #-}
instance                  MonadTrans  (Listener t cfg)   where lift       = wrap'                                ; {-# INLINE lift   #-}
instance Applicative m => Applicative (Listener t cfg m) where pure       = wrap' ∘ pure                         ; {-# INLINE pure   #-}
                                                               (<*>)  f a = wrap' $ unwrap' f <*> unwrap' a      ; {-# INLINE (<*>)  #-}

-- Registration time type constraint

instance {-# OVERLAPPABLE #-} (Monad m, Register t a m)                    => Register t a (Listener t' cfg m) where register_     = lift ∘∘ register_
instance {-# OVERLAPPABLE #-} (Monad m, Register t a m, Handler t cfg m a) => Register t a (Listener t  cfg m) where register_ t a = handler a *> (lift $ register_ t a)





-----------------------------
-- === Type Constraint === --
-----------------------------

-- === Definitions === ---

data TypeConstraint (ctx :: * -> * -> Constraint) tp
instance (ctx a tp, Monad m) => Handler t (TypeConstraint ctx tp) m a where handler _ = return () ; {-# INLINE handler #-}


-- === Constraint rules === ---

class Equality_Full a b
instance a ~ b => Equality_Full a b

class Equality_M1 a b
instance (a ~ ma pa, b ~ mb pb, ma ~ mb) => Equality_M1 a b

class Equality_M2 a b
instance (a ~ m1a (m2a pa), b ~ m1b (m2b pb), m1a ~ m1b, m2a ~ m2b) => Equality_M2 a b 

class Equality_M3 a b
instance (a ~ m1a (m2a (m3a pa)), b ~ m1b (m2b (m3b pb)), m1a ~ m1b, m2a ~ m2b, m3a ~ (m3b :: ([*] -> *) -> *)) => Equality_M3 a b 
-- FIXME[WD]: remove the kind constraint above


-- === Utils === ---

constrainType :: Proxy ctx -> t -> Proxy tp -> Listener t (TypeConstraint ctx tp) m a -> m a
constrainType _ _ _ = unwrap'

constrainTypeEq :: t -> Proxy tp -> Listener t (TypeConstraint Equality_Full tp) m a -> m a
constrainTypeM1 :: t -> Proxy tp -> Listener t (TypeConstraint Equality_M1   tp) m a -> m a
constrainTypeM2 :: t -> Proxy tp -> Listener t (TypeConstraint Equality_M2   tp) m a -> m a
constrainTypeM3 :: t -> Proxy tp -> Listener t (TypeConstraint Equality_M3   tp) m a -> m a
constrainTypeEq = constrainType (p :: P Equality_Full)
constrainTypeM1 = constrainType (p :: P Equality_M1)
constrainTypeM2 = constrainType (p :: P Equality_M2)
constrainTypeM3 = constrainType (p :: P Equality_M3)





----------------------
-- === Register === --
----------------------
-- | The `register` function can be used to indicate that a particular element is "done".
--   It does not provide any general special meaning. In general, this information can be lost when not used explicitly.
--   For a specific usage look at the `Network` builder, where `register` is used to add type constrains on graph nodes and edges.
--   The `t` parameter is the type of registration, like `Node` or `Edge`. Please keep in mind, that `Node` indicates a "kind" of a structure.
--   It does not equals a graph-like node - it can be a "node" in flat AST representation, like just an ordinary term.



class Monad m => Register t a m where 
    register_ :: t -> a -> m ()


-- === Utils === --

registerM :: Register t a m => t -> m a -> m a
registerM t ma = do
    a <- ma
    register_ t a
    return a
{-# INLINE registerM #-}

register :: Register t a m => t -> a -> m a
register t a = a <$ register_ t a ; {-# INLINE register #-}


-- === Instances === --

instance {-# OVERLAPPABLE #-}
         (Register t a m, MonadTrans f, Monad m, Monad (f m)) => Register t a (f m)    where register_     = lift ∘∘ register_ ; {-# INLINE register_ #-}
instance                                                         Register t a IO       where register_ _ _ = return ()         ; {-# INLINE register_ #-}
instance                                                         Register t a Identity where register_ _ _ = return ()         ; {-# INLINE register_ #-}







