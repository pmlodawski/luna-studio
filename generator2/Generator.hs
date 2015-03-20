{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Generator where

import Control.Applicative
import Text.Printf
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (VarStrictType)
import qualified Language.Haskell.TH.Syntax as THS
import Language.Haskell.TH.Quote
import Data.List
import qualified Data.Set as Set

import Debug.Trace


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

translateToCppName :: Name -> String 
translateToCppName name = nameBase name

generateRootClassWrapper :: Dec -> CppClass
generateRootClassWrapper (DataD cxt name tyVars cons names) = 
	CppClass (translateToCppName name) [] [] []

typeOfField :: Type -> Q String
typeOfField (ConT name) = 
	let nb = nameBase name
	in return $ if nb == "String" then "std::string" else nb

typeOfField (AppT ListT (nested)) = do
	nestedType <- typeOfField nested
	return $ printf "std::vector<%s>" $ nestedType
--typeOfField (AppT ConT (maybe)) = printf "boost::optional<%s>" $ typeOfField nested
typeOfField t = return $ "[" ++ show t ++ "]"

processField :: THS.VarStrictType -> Q CppField
processField field@(name, _, t) = do
	filedType <- typeOfField t
	return $ CppField (translateToCppName name) filedType

processConstructor :: Con -> Name -> Q CppClass
processConstructor con@(RecC cname fields) base = 
	do
		let baseCppName = translateToCppName base
		    derCppName = baseCppName ++ "_" ++ translateToCppName cname
		cppFields <- mapM processField fields
		return $ CppClass derCppName cppFields [] [CppDerive baseCppName False Public]

generateCppWrapperHlp :: Dec -> Q CppParts
generateCppWrapperHlp dec@(DataD cxt name tyVars cons names) = 
	do
		let baseClass = generateRootClassWrapper dec
		derClasses <- sequence $ processConstructor <$> cons <*> [name]
		-- derClasses = processConstructor <$> cons <*> [name]
		let classes = baseClass : derClasses
		    functions = []
		return (CppParts classes functions)

generateCppWrapper :: Info -> Q (String, String)
generateCppWrapper (TyConI dec@(DataD cxt name tyVars cons names)) = 
	do
		cppParts <- generateCppWrapperHlp dec
		let cppFormattedParts = formatCpp cppParts
		return cppFormattedParts --fst cppFormattedParts ++ " \n$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n " ++ snd cppFormattedParts
--generateCppWrapper (DataConI n t p f) = ("ggggggggg", show n)
--generateCppWrapper exp = ("barrrrrr","baz")

class TypesDependencies a where
	listDependencies :: a -> Q [Name]

instance TypesDependencies Type where
	listDependencies (ConT name) = do
		nestedDeps <- collectDependencies name
		return $ nub $ [name] ++ nestedDeps
	listDependencies (AppT lhs rhs) = do
		list1 <- listDependencies lhs
		list2 <- listDependencies rhs
		return $ nub $ list1 ++ list2
	listDependencies ListT = return []
	listDependencies arg = trace (show arg) (return [])

instance TypesDependencies VarStrictType where
	listDependencies (_, _, t) = listDependencies t

instance TypesDependencies a => TypesDependencies [a] where
	listDependencies listToProcess = do
		list <- mapM listDependencies listToProcess
		return $ nub $ concat list

instance TypesDependencies Con where
	listDependencies con@(RecC cname fields) = listDependencies fields
	listDependencies arg = return []

instance TypesDependencies Info where
	listDependencies (TyConI (DataD _ n _ cons _)) = listDependencies cons
	listDependencies (TyConI (TySynD n _ t)) = listDependencies t
	listDependencies arg = trace (show arg) (return [])

collectDependencies :: Name -> Q [Name]
collectDependencies name = do
	info <- reify name
	listDependencies info

printAst :: Info -> String
printAst  (TyConI dec@(DataD cxt name tyVars cons names)) = 
	let namesShown = (Prelude.map show names) :: [String]
	    consCount = Data.List.length cons :: Int
	    ret = ("cxt=" ++ show cxt ++ "\nname=" ++ show name ++ "\ntyVars=" ++ show tyVars ++ "\ncons=" ++ show cons ++ "\nnames=" ++ show names) :: String
	in show consCount ++ "___" ++ ret

fileContents :: Q String
fileContents = return "blah"

generateCpp :: Name -> FilePath -> Q Exp
generateCpp name path = do
	let headerName = path ++ ".h"
	let cppName = path ++ ".cpp"

	reifiedName <- reify name
	(header,body) <- generateCppWrapper reifiedName

	dependencies <- collectDependencies name
	runIO (putStrLn $ printf "Found %d dependencies: %s" (length dependencies) (show dependencies))

	runIO (writeFile headerName header)
	runIO (writeFile cppName body)

	runIO (writeFile cppName (printf "#include \"%s\"" headerName))
	[|  return () |]

