{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Proxy.Utils where

import Data.Typeable

toProxy :: a -> Proxy a
toProxy _ = Proxy
