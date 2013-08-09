{-# LANGUAGE FunctionalDependencies,
             FlexibleInstances,
             TemplateHaskell,
             UndecidableInstances #-}

module Workspace'.Vector (
    Vector(..),
    len
) where

import Common'.F_len
import Common'.F_getx
import Flowbox'.Core
import qualified Workspace'.Vector.U'len

data Vector a = Vector{
    x'F :: a
} deriving (Show)

getx'T    = x'F
getx'T''M = return . getx'T

setx'T k v = k{x'F = v}
setx'T''M  = return (.:) setx'T

mkInstIO ''F_getx 'getx'T                      'getx'T''M                      'getx
mkInstIO ''F_len  'Workspace'.Vector.U'len.len 'Workspace'.Vector.U'len.len''M 'len

