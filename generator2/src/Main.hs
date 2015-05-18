{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}


import Generator.Generator
import Generator.FFI
import Generator.Expr
import System.FilePath
-- import Data.Binary
import Data.Binary
-- import Generator.Binary.Class

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQ

import qualified Generator.AST.Lit


import Data.ByteString.Lazy (ByteString)

instance Binary Generator.AST.Lit.Lit
instance Binary Accessor
instance Binary Expr
instance Binary Name
instance Binary a => Binary (Arg a)

outputDirectory = ".." </> ".." </> "sample_deserializer"

encodeWithFname fname val = encodeFile (outputDirectory </> fname) val

-- $(generateFFI 'adjust)

--moja :: Int -> Int
--moja a = a * 43

-- $(generateFFI 'moja)

--mmm a = Main.moja' a

--ttt :: ByteString -> ByteString
--ttt b = Main.adjust' b

main = do
    putStrLn "Generator 2 im. Arystotelesa."
    let con0 = Con 100 "[fooBarBazBarfooBarBazBarfooBarBazBarfooBarBazBar]"
    let args = [NestingEvil [con0]] :: [Arg Expr]
    let con = App 1200 (TypeDef 76 "typ1" "Typ2") args
    let imp = Import 4613 ["foo1 日本語ショック！", "foo2", "foo3", "foo4"] con (Just "opcjonalny tekst")
    let argexp = Arg 4321 (0) (Just imp)
    let lit = Lit 500 ((Generator.AST.Lit.IntLit 400), 50, "", con)

    let acc = Accessor 503 (ConAccessor "bar") (Con 502 "foo")

    let ref = Ref 568  (1, 2.5, Just [NOP 50], (5, "pięć"))
    --let bs = encode con

    encodeWithFname "test.bin" con
    encodeWithFname "testimp.bin" imp
    encodeWithFname "testargexp.bin" argexp
    encodeWithFname "testlit.bin" lit
    encodeWithFname "testacc.bin" acc
    encodeWithFname "testref.bin" ref

    --putStrLn $ show $ encode acc

    --encodeFile "../../sample_deserializer/testname.bin" (NameA "Foo blah" 34864296 500.750)

    --putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
    --ns <- decodeFile "../../sample_deserializer/testout.bin" :: IO Expr
    --putStrLn $ show ns
    --putStrLn "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"

    ---- $(THQ.dataToExpQ (const Nothing) (formatCppWrapper ''Expr))
    ---- putStrLn $(TH.stringE . printAst =<< TH.reify ''Expr)
    ---- putStrLn $(TH.stringE . TH.pprint =<< TH.reify ''Expr)

    $(generateCpp ''Expr "../../sample_deserializer")
