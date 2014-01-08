{-# LANGUAGE PolyKinds #-}
-- there is a bug, when reifying functions if PolyKind was enabled in some type classes module.

module FlowboxM.Luna.Proxy where

data P a = P
