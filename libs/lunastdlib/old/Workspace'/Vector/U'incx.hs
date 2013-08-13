-- This is Flowbox generated file.

module Workspace'.Vector.U'incx where

-- imports
import           Common'.C''add        
import           Common'.C''select0    
import           Common'.C''x'getter   
import           Common'.C''x'setter   
import           Flowbox'.Core         

-- datatypes


-- functions
incx inputs' = 
    let
        v'5 = 1 :: Int
        v'0 = inputs'
        v'1 = select0 v'0
        v'2 = (v'1, ())
        v'3 = x'getter v'2
        v'4 = select0 v'3
        v'6 = (v'5, (v'4, ()))
        v'7 = add v'6
        v'8 = select0 v'7
        v'9 = (v'1, (v'8, ()))
        v'10 = x'setter v'9
        outputs' = v'10
    in outputs'

incx''M inputs' = return $ incx inputs'

-- expressions