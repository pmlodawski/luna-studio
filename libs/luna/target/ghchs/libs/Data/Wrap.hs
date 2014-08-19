{-# LANGUAGE DefaultSignatures #-}

module Data.Wrap where

import Control.Monad.Trans

class Wrap m where
    wrap   :: a   -> m a
    unwrap :: m a -> a

    default wrap :: Monad m => a -> m a
    wrap = return

class WrapT t where
    wrapT   :: m a   -> t m a
    unwrapT :: t m a -> m a

    default wrapT :: (MonadTrans t, Monad m) => m a -> t m a
    wrapT = lift




