{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Proxy.Utils where

import Data.Typeable
import Prelude

toProxy :: a -> Proxy a
toProxy _ = Proxy

proxyTypeName :: Typeable a => Proxy a -> String
proxyTypeName = drop 6 . show . typeOf
