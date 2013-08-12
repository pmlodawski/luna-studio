-- This is Flowbox generated file.

{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Workspace'.Vector where

-- imports
import           Common'.C''incx            
import           Common'.C''x'getter        
import           Common'.C''x'setter        
import           Common'.C''y'getter        
import           Common'.C''y'setter        
import           Common'.C''z'getter        
import           Common'.C''z'setter        
import           Common'.C''select0         --
import           Flowbox'.Core              
import qualified Workspace'.Vector.U'incx   

-- datatypes
data Vector a = Vector {x'F :: a, y'F :: a, z'F :: a} deriving (Show)

-- functions
incx'T inputs'@(Vector {}, _) = Workspace'.Vector.U'incx.incx inputs'
incx'T''M inputs'@(Vector {}, _) = Workspace'.Vector.U'incx.incx''M inputs'

-- expressions
mkInst'' ''C''incx 'incx'T 'incx'T''M 'incx

instance C''x'getter (Vector a,()) (a,()) where
    x'getter v = (x'F (select0 v),())
    x'getter''M = return . x'getter



instance C''x'setter (Vector a, (a, ())) (Vector a, ()) where
    x'setter (k, (v, ()))   = (k{x'F = v}, ())
    x'setter''M args = return $ x'setter args




---- This is Flowbox generated file.

--{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

--module Workspace'.Vector where

---- imports
--import Common'.C''incx
--import Common'.C''x'getter
--import Common'.C''x'setter
--import Common'.C''y'getter
--import Common'.C''y'setter
--import Common'.C''z'getter
--import Common'.C''z'setter
--import Common'.C''select0
--import Flowbox'.Core
--import qualified Workspace'.Vector.U'incx
--import Data.Tuple.Update



--data Vector a = Vector { x'F :: a, y'F :: a, z'F :: a } deriving (Show)
--data Vector2 a = Vector2 { x'F2 :: a, y'F2 :: a, z'F2 :: a } deriving (Show)



--instance C''x'getter (Vector a,()) (a,()) where
--    x'getter v = (x'F (select0 v),())
--    x'getter''M = return . x'getter



--instance C''x'setter (Vector a, (a, ())) (Vector a, ()) where
--    x'setter (k, (v, ()))   = (k{x'F = v}, ())
--    x'setter''M args = return $ x'setter args


----incx_''M inputs' = return $ incx inputs'


--myincx    inputs'@(Vector{},_) = Workspace'.Vector.U'incx.incx    inputs'
--myincx''M inputs'@(Vector{},_) = Workspace'.Vector.U'incx.incx''M inputs'


----myincx2    inputs'@(OneTuple Vector2{}) = Workspace'.Vector.U'incx.incx    inputs'
----myincx2''M inputs'@(OneTuple Vector2{}) = Workspace'.Vector.U'incx.incx''M inputs'

--mkInstIO ''C''incx 'myincx 'myincx''M 'incx


----mkInstIO ''C''incx 'myincx2 'myincx2''M 'incx

----mkInstIO ''C''incx 'incx_ 'incx_''M 'incx

----mkInstIO ''C''incx 'Workspace'.Vector.U'incx.incx 'Workspace'.Vector.U'incx.incx''M 'incx

----instance C''incx (OneTuple (Vector a)) a where
----      incx = Workspace'.Vector.U'incx.incx
----      incx''M = Workspace'.Vector.U'incx.incx''M 


----instance C''select0 a (Vector b) =>
----             C''incx a b where
----      incx = incx_
----      incx''M = incx_''M

----instance C''select0 a (Vector b) =>
----             C''incx a b where
----      incx = incx_
----      incx''M = incx_''M


----instance C''select0 a (Vector2 b) =>
----             C''incx a b where
----      incx = incx_
----      incx''M = incx_''M

----instance (C''x'getter (OneTuple a) b,
----              C''select0 c a,
----              C''select0 b d) =>
----             C''incx c d where
----      incx = Workspace'.Vector.U'incx.incx
----      incx''M = Workspace'.Vector.U'incx.incx''M