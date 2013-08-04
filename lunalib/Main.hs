import qualified Workspace'.Vector as Vector
import           Workspace'.Vector   (Vector(..))

import Data.Tuple.OneTuple
import Common'.C''incx
import Flowbox'.Data.Tuple.Select

main = do
	let
		x = 1::Int
		y = 2::Int
		z = 3::Int
		v = Vector x y z
		v2 = incx $ OneTuple v
		--l = Vector.len v
		
	print v2
	return ()
