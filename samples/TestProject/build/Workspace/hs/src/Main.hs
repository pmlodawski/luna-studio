-- handwritten
import Workspace
import Workspace.Vector
import Flowbox.Luna.FClasses.U'incx
import Flowbox.Luna.FClasses.U'init
import Data.Default

main = do
	let 
		x :: Int
		x = 1
		y :: Int
		y = 1
		z :: Int 
		z = 1
		--q = def :: Vector a
		--a = def -- :: Vector Int
		--b = a { x'F = 5 }
		--a = Vector {}
		a = def :: Vector Int
		b = init' (a,(x,(y,(z,()))))
		--b = incx' (a, ())
	print b
	return ()
