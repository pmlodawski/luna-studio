{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

import Generator
import Expr

import Data.Binary
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQ

instance Binary Accessor
instance Binary Expr
instance Binary Name
instance Binary a => Binary (Arg a)

main = do
	putStrLn "Generator 2 im. Arystotelesa."
	let con0 = Con 100 "[fooBarBazBarfooBarBazBarfooBarBazBarfooBarBazBar]"
	let args = [NestingEvil [con0]] :: [Arg Expr]
	let con = App 1200 (TypeDef 76 "typ1" "Typ2") args
	let imp = Import 4613 ["foo1", "foo2", "foo3", "foo4"] con (Just "opcjonalny tekst")
	let argexp = Arg 4321 (0) (Just imp)
	let bs = encode con

	encodeFile "../sample_deserializer/test.bin" con
	encodeFile "../sample_deserializer/testimp.bin" imp
	encodeFile "../sample_deserializer/testargexp.bin" argexp

	let acc = Accessor 503 (ConAccessor "bar") (Con 502 "foo")
	putStrLn $ show $ encode acc
	encodeFile "testacc.bin" acc

	encodeFile "../sample_deserializer/testname.bin" (NameA "Foo blah" 34864296 123456789)

	putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
	ns <- decodeFile "../sample_deserializer/testout.bin" :: IO Expr
	putStrLn $ show ns
	putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"

	-- $(THQ.dataToExpQ (const Nothing) (formatCppWrapper ''Expr))
	-- putStrLn $(TH.stringE . printAst =<< TH.reify ''Expr)
	-- putStrLn $(TH.stringE . TH.pprint =<< TH.reify ''Expr)

	$(generateCpp ''Expr "../sample_deserializer/generated")
