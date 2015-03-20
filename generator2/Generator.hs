{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Generator where

import Expr

import Control.Applicative
import Text.Printf
import GHC.Generics
import Text.XML.ToFromXML
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS
import Language.Haskell.TH.Quote
import Data.List



type HeaderSource = String

type ImplementationSource = String

class CppFormattablePart a where
	format :: a -> String

class CppFormattable a where
	formatCpp :: a -> (HeaderSource, ImplementationSource)

data CppArg = CppArg 
	{ argName :: String
	, argType :: String	
	}

instance CppFormattablePart CppArg where
	format arg = argType arg ++ " " ++ argName arg

data CppQualifiers = None | Const | Volatile | ConstVolatile

data CppFunction = CppFunction
	{ name :: String
	, returnType :: String
	, args :: [CppArg]
	, body :: String
	}


formatArgsList :: [CppArg] -> String
formatArgsList args = "(" ++ Data.List.intercalate ", " (map format args) ++ ")"

formatSignature :: CppFunction -> String
formatSignature (CppFunction name ret args _) = formatArgsList args

data CppMethod = CppMethod
	{ function :: CppFunction
	, qualifiers :: CppQualifiers
	}

data CppField = CppField 
    { fieldName :: String
	, fieldType :: String
	}

instance CppFormattablePart CppField where
	format field = fieldType field ++ " " ++ fieldName field

instance CppFormattablePart [CppField] where
	format fields = 
		let formatField field = printf "\t%s;" (format field) -- FIXME think think think
		    formattedFields = formatField <$> fields
		    ret = intercalate "\n" formattedFields
		in ret

data CppAccess = Protected | Public | Private
instance CppFormattablePart CppAccess where
	format Protected = "protected"
	format Public = "public"
	format Private = "private"

data CppDerive = CppDerive
	{ baseName :: String
	, isVirtual :: Bool
	, access :: CppAccess
	}

instance CppFormattablePart CppDerive where
	format (CppDerive base virtual access) = format access ++ " " ++ (if virtual then "virtual " else "") ++ base

instance CppFormattablePart [CppDerive] where
	format [] = ""
	format derives = ": " ++ Data.List.intercalate ", " (map format derives)

data CppClass = CppClass 
	{ className :: String
	, classFields :: [CppField]
	, classMethods :: [CppMethod]
	, baseClasses :: [CppDerive]
	}


instance CppFormattable CppClass where
	formatCpp (CppClass name fields methods bases) = 
		let headerCode = printf "class %s %s \n{\npublic:\n%s\n};" name (format bases) (format fields)
		    bodyCode = ""
		in (headerCode, bodyCode)

data CppParts = CppParts
	{ classes :: [CppClass]
	, functions :: [CppFunction]
	}

instance CppFormattable CppParts where
	formatCpp (CppParts cs fns) = 
		let pairs = Data.List.map formatCpp cs
		    collectCodePieces fn = Data.List.intercalate "\n\n/****************/\n\n" (map fn pairs)
		    headerCode = collectCodePieces fst
		    bodyCode = collectCodePieces snd
		in (headerCode, bodyCode)

translateToCppName :: TH.Name -> String 
translateToCppName name = TH.nameBase name

generateRootClassWrapper :: TH.Dec -> CppClass
generateRootClassWrapper (TH.DataD cxt name tyVars cons names) = 
	CppClass (translateToCppName name) [] [] []

typeOfField :: TH.Type -> String
typeOfField (TH.ConT name) = 
	let nb = TH.nameBase name
	in if nb == "String" then "std::string" else nb
	
	-- let tinfo =  
	-- in "<" ++ show name ++ ">"
typeOfField (TH.AppT TH.ListT (nested)) = printf "std::vector<%s>" $ typeOfField nested
--typeOfField (TH.AppT TH.ConT (maybe)) = printf "boost::optional<%s>" $ typeOfField nested
typeOfField t = "[" ++ show t ++ "]"

processField :: THS.VarStrictType -> CppField
processField field@(name, _, t) = CppField (translateToCppName name) (typeOfField t)

processConstructor :: TH.Con -> TH.Name -> CppClass
processConstructor con@(TH.RecC cname fields) base = 
	let baseCppName = translateToCppName base
	    derCppName = baseCppName ++ "_" ++ translateToCppName cname
	    cppFields = processField <$> fields
	in CppClass derCppName cppFields [] [CppDerive baseCppName False Public]

generateCppWrapperHlp :: TH.Dec -> CppParts
generateCppWrapperHlp dec@(TH.DataD cxt name tyVars cons names) = 
	let baseClass = generateRootClassWrapper dec
	    derClasses = processConstructor <$> cons <*> [name]
	    classes = baseClass : derClasses
	    functions = []
	in CppParts classes functions

generateCppWrapper :: TH.Info -> (String, String)
generateCppWrapper (TH.TyConI dec@(TH.DataD cxt name tyVars cons names)) = 
	let cppParts = generateCppWrapperHlp dec
	    cppFormattedParts = formatCpp cppParts
	in cppFormattedParts --fst cppFormattedParts ++ " \n$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n " ++ snd cppFormattedParts
generateCppWrapper (TH.DataConI n t p f) = ("ggggggggg", show n)
generateCppWrapper exp = ("barrrrrr","baz")


printAst :: TH.Info -> String
printAst  (TH.TyConI dec@(TH.DataD cxt name tyVars cons names)) = 
	let namesShown = (Prelude.map show names) :: [String]
	    consCount = Data.List.length cons :: Int
	    ret = ("cxt=" ++ show cxt ++ "\nname=" ++ show name ++ "\ntyVars=" ++ show tyVars ++ "\ncons=" ++ show cons ++ "\nnames=" ++ show names) :: String
	in show consCount ++ "___" ++ ret

fileContents :: TH.Q String
fileContents = return "blah"

generateCpp :: TH.Name -> FilePath -> TH.Q TH.Exp
generateCpp name path = do
	let headerName = path ++ ".h"
	let cppName = path ++ ".cpp"

	reifiedName <- TH.reify name
	let (header,body) = generateCppWrapper reifiedName

	TH.runIO (writeFile headerName header)
	TH.runIO (writeFile cppName body)

	TH.runIO (writeFile cppName (printf "#include \"%s\"" headerName))
	[|  return () |]



-- main = do
-- 	let test = Test "abc" (42,'z') :: Test
-- 	let shownTest = (show test)::String
-- 	-- Prelude.putStrLn $(TH.stringE . generateCppWrapper =<< TH.reify ''Expr)
-- 	Prelude.putStrLn $(TH.stringE . TH.pprint =<< TH.reify ''Expr)

-- 	process ''Bool

-- 	-- Prelude.putStrLn [|testowa|]

-- 	--dataInfo <- inmq
-- 	Data.ByteString.Char8.putStrLn $ toXML $ test
-- 	Prelude.putStrLn "Hello World"
-- 	Prelude.putStrLn (dump $ Just test)
-- 	Prelude.putStrLn (dump $ (Nothing::Maybe Int))
-- 	Prelude.putStrLn $ show moja
-- 	-- Prelude.putStrLn (dump $ (Nothing::Maybe Int))
-- 	-- return 
-- 	-- Prelude.putStrLn $ show $ TH.reify ''Data
