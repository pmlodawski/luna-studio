--{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

--import Flowbox'.Core
--import Common'.C''select0
--import Common'.C''incx
--import Common'.C''x'getter

--import qualified Workspace'.Vector as Vector
--import           Workspace'.Vector   (Vector(..))
--import qualified Workspace'.Vector.U'incx
----import qualified Workspace'.Vector2 as Vector2
----import           Workspace'.Vector2   (Vector2(..))

--main = do
--	let
--		x = 1::Int
--		y = 2::Int
--		z = 3::Int
--		v = Vector x y z
--		v2 = incx $ (v,())

--	print v2
--	return ()



-- imports
import           Common'.C''print          
import           Flowbox'.Core             
import           Flowbox'.System.Console   (Console(..))
import qualified Flowbox'.System.Console as Console
import           Prelude(return)           

-- datatypes


-- functions
mymain inputs' = 
    let
        v'4 = "hello world!"
        v'2 = Console
        v'3 = v'2
        v'5 = (v'3, (v'4, ()))
        v'6 = print v'5
        outputs' = ()
        v'0 = inputs'
    in outputs'

mymain''M inputs' = do
    let
        v'4 = "hello world!"
        v'2 = Console
        v'3 = v'2
        v'5 = (v'3, (v'4, ()))
    v'6 <- print''M v'5
    let
        outputs' = ()
        v'0 = inputs'
    return outputs'


main = mymain''M ()