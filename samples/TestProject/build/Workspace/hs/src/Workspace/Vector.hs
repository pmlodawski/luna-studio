-- This is Flowbox generated file.

{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Workspace.Vector where

-- imports
import Flowbox.Luna.FClasses.U'incx
import Flowbox.Luna.FClasses.U'init
import Flowbox.Luna.FClasses.U'select0
import Flowbox.Luna.FClasses.U'x'getter
import Flowbox.Luna.FClasses.U'x'setter
import Flowbox.Luna.FClasses.U'y'getter
import Flowbox.Luna.FClasses.U'y'setter
import Flowbox.Luna.FClasses.U'z'getter
import Flowbox.Luna.FClasses.U'z'setter
import Flowbox.Luna.Helpers.Core
import qualified Workspace.Vector.U'incx
import qualified Workspace.Vector.U'init

-- datatypes
data Vector a = Vector {x'F :: a, y'F :: a, z'F :: a} deriving (Show)

-- functions
incx'T inputs''@(Vector {}, _) = Workspace.Vector.U'incx.incx' (inputs'')
incx'T''M inputs''@(Vector {}, _) = Workspace.Vector.U'incx.incx'''M (inputs'')
init'T inputs''@(Vector {}, _) = Workspace.Vector.U'init.init' (inputs'')
init'T''M inputs''@(Vector {}, _) = Workspace.Vector.U'init.init'''M (inputs'')

-- expressions
mkInst'' (''C''incx') ('incx'T) ('incx'T''M) ('incx')
mkInst'' (''C''init') ('init'T) ('init'T''M) ('init')
mkSetter ("x") (''Vector)
mkSetter ("y") (''Vector)
mkSetter ("z") (''Vector)
mkGetter ("x") (''Vector)
mkGetter ("y") (''Vector)
mkGetter ("z") (''Vector)

