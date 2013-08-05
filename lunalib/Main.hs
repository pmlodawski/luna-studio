--import qualified Workspace'.Vector as Vector
--import           Workspace'.Vector   (Vector(..))

--import Data.Tuple.OneTuple
--import Common'.C''incx
--import Flowbox'.Data.Tuple.Select

--main = do
--	let
--		x = 1::Int
--		y = 2::Int
--		z = 3::Int
--		v = Vector x y z
--		--v2 :: OneTuple Int
--		v2 = incx $ OneTuple v
--		--l = Vector.len v

--	print v2
--	return ()


{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

import Flowbox'.Core
import Common'.C''select0
import Common'.C''incx
import Common'.C''x'getter

import qualified Workspace'.Vector as Vector
import           Workspace'.Vector   (Vector(..))
import qualified Workspace'.Vector.U'incx
--import qualified Workspace'.Vector2 as Vector2
--import           Workspace'.Vector2   (Vector2(..))

main = do
	let
		x = 1::Int
		y = 2::Int
		z = 3::Int
		v = Vector x y z
		v2 = incx $ (v,())

	print v2
	return ()