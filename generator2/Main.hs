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
	let con = Con 100 "[fooBarBazBarfooBarBazBarfooBarBazBarfooBarBazBar]"
	let bs = encode con

	encodeFile "test.bin" bs

	putStrLn $(TH.stringE . printAst =<< TH.reify ''Expr)
	putStrLn $(TH.stringE . TH.pprint =<< TH.reify ''Expr)

	$(generateCpp ''Expr "../sample_deserializer/generated")
