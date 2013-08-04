-- This is Flowbox generated file.

{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Workspace'.Vector where

-- imports
import Common'.C''incx
import Common'.C''x'getter
import Common'.C''x'setter
import Common'.C''y'getter
import Common'.C''y'setter
import Common'.C''z'getter
import Common'.C''z'setter
import Flowbox'.Core
import qualified Workspace'.Vector.U'incx
import Data.Tuple.OneTuple -- handcode

-- datatypes
data Vector a = Vector { x'F :: a, y'F :: a, z'F :: a } deriving (Show)

-- functions


-- expressions
instance C''x'getter (Vector a) (OneTuple a) where
    x'getter v = OneTuple $ x'F v
    x'getter''M = return . x'getter

instance C''x'setter (Vector a, a) (Vector a) where
    x'setter (k, v)   = k{x'F = v}
    x'setter''M (k, v)= return $ x'setter (k, v)

mkInstIO ''C''incx 'Workspace'.Vector.U'incx.incx 'Workspace'.Vector.U'incx.incx''M 'incx
--setter'' ''Vector 'x'F
--setter'' ''Vector 'y'F
--setter'' ''Vector 'z'F
--getter'' ''Vector 'x'F
--getter'' ''Vector 'y'F
--getter'' ''Vector 'z'F


