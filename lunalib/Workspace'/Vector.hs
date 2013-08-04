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
import Common'.C''select0
import Flowbox'.Core
import qualified Workspace'.Vector.U'incx
import Data.Tuple.OneTuple -- handcode

-- datatypes
data Vector a = Vector { x'F :: a, y'F :: a, z'F :: a } deriving (Show)

-- functions

instance (C''select0 a (Vector b)) => C''x'getter a (OneTuple b) where
    x'getter v = OneTuple $ x'F (select0 v)
    x'getter''M = return . x'getter


---- expressions
--instance C''x'getter (Vector a) (OneTuple a) where
--    x'getter v = OneTuple $ x'F v
--    x'getter''M = return . x'getter

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


--instance (C''x'getter (OneTuple a) b,
--          C''select0 c a,
--          C''select0 b d) 
--          =>
--          C''incx c b where
--    incx = Workspace'.Vector.U'incx.incx
--    incx''M = Workspace'.Vector.U'incx.incx''M

--instance (C''x'getter (OneTuple a) b,
--          C''select0 c a,
--          C''select0 b d) 
--          =>
--          C''incx c d where
--    incx = Workspace'.Vector.U'incx.incx
--    incx''M = Workspace'.Vector.U'incx.incx''M
