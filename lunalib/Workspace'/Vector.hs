-- This is Flowbox generated file.

{-# LANGUAGE TemplateHaskell #-}

module Workspace'.Vector where

-- imports
import Common'.C''incx
import Common'.C''x'Getter
import Common'.C''x'Setter
import Common'.C''y'Getter
import Common'.C''y'Setter
import Common'.C''z'Getter
import Common'.C''z'Setter
import Flowbox'.Core
import qualified Workspace'.Vector.U'incx

-- datatypes
data Vector a = Vector { x'F :: a, y'F :: a, z'F :: a }

-- functions


-- expressions
incx'T = Workspace'.Vector.incx.incx
incx''M'T = Workspace'.Vector.incx.incx''M
mkInst'' incx'T incx''M'T incx
setter'' Vector x'F
setter'' Vector y'F
setter'' Vector z'F
getter'' Vector x'F
getter'' Vector y'F
getter'' Vector z'F
