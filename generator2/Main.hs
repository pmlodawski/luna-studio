{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

import Generator
import Expr

import GHC.Generics
import Text.XML.ToFromXML
-- import Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Char8
import Data.Binary
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import GHC.Generics (Generic)

instance Binary Accessor
instance Binary Expr

main = do
	putStrLn "Generator 2 im. Arystotelesa."
	let con = Con 100 "[fooBarBazBarfooBarBazBarfooBarBazBarfooBarBazBar]"
	let bs = (encode con) :: Data.ByteString.Lazy.ByteString 

	encodeFile "test.bin" bs

	$(generateCpp ''Expr "generated")

	-- Prelude.putStrLn $(TH.stringE . (fst generateCppWrapper =<< TH.reify ''Expr))
	putStrLn $(TH.stringE . printAst =<< TH.reify ''Expr)
	putStrLn $(TH.stringE . TH.pprint =<< TH.reify ''Expr)
