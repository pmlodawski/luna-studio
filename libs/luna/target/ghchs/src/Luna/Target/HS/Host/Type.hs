{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Target.HS.Host.Type where

import           Prelude hiding (any)

ofType :: a -> a -> a
ofType = const

anyType :: a
anyType = error "any undefined type"


