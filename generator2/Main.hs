{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

import Generator
import Expr

import Data.Binary
import qualified Language.Haskell.TH as TH

instance Binary Accessor
instance Binary Expr
instance Binary a => Binary (Arg a)

main = do
	putStrLn "Generator 2 im. Arystotelesa."
	let con0 = Con 100 "[fooBarBazBarfooBarBazBarfooBarBazBarfooBarBazBar]"
	let args = [NestingEvil [con0]] :: [Arg Expr]
	let con = App 1200 (TypeDef 76 "typ1" "Typ2") args
	let bs = encode con

	encodeFile "../sample_deserializer/test.bin" con

	let acc = Accessor 503 (ConAccessor "bar") (Con 502 "foo")
	putStrLn $ show $ encode acc
	encodeFile "testacc.bin" acc

	putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
	ns <- decodeFile "../sample_deserializer/testout.bin" :: IO Expr
	putStrLn $ show ns
	putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"

	-- putStrLn $(TH.stringE . printAst =<< TH.reify ''Expr)
	-- putStrLn $(TH.stringE . TH.pprint =<< TH.reify ''Expr)

	$(generateCpp ''Expr "../sample_deserializer/generated")
