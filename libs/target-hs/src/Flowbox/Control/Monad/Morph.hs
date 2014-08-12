{-# LANGUAGE MultiParamTypeClasses #-}

module Flowbox.Control.Monad.Morph where

class MonadMorph m n where
    morph :: m a -> n a

