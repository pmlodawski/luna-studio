{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Generator where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Control.Applicative
import Text.Printf
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (VarStrictType)
import qualified Language.Haskell.TH.Syntax as THS
import Language.Haskell.TH.Quote
import Data.List
import qualified Data.Set as Set

import GHC.Stack
import Debug.Trace


type HeaderSource = String

type ImplementationSource = String

type CppFormattedCode = (HeaderSource, ImplementationSource)

class CppFormattablePart a where
    format :: a -> String

class CppFormattable a where
    formatCpp :: a -> CppFormattedCode

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

instance CppFormattable CppFunction where
    formatCpp fn = ("function fundc", "function fdunc")

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

data CppInclude = CppSystemInclude String | CppLocalInclude String
instance CppFormattable CppInclude where
    formatCpp (CppSystemInclude path) = (printf "#include <%s>" path, "")
    formatCpp (CppLocalInclude path) = (printf "#include \"%s\"" path, "")

data CppForwardDecl = CppForwardDeclClass String | CppForwardDeclStruct String
instance CppFormattable CppForwardDecl where
    formatCpp (CppForwardDeclClass name) = (printf "class %s;" name, "")
    formatCpp (CppForwardDeclStruct name) = (printf "struct %s;" name, "")

data CppTypedef  = CppTypedef 
    { introducedType :: String
    , baseType :: String
    }

instance CppFormattable CppTypedef where
    formatCpp (CppTypedef to from) = (printf "typedef %s %s;" from to, "")

data CppParts = CppParts
    { includes :: [CppInclude]
    , forwardDecls :: [CppForwardDecl]
    , typedefs :: [CppTypedef]
    , classes :: [CppClass]
    , functions :: [CppFunction]
    }

instance CppFormattable CppParts where
    formatCpp (CppParts incl frwrds tpdefs cs fns) = 
        let includesPieces = map formatCpp incl
            forwardDeclPieces = map formatCpp frwrds
            typedefPieces = map formatCpp tpdefs
            classesCodePieces = map formatCpp cs
            functionsPieces = map formatCpp fns
            -- FIXME code duplication above

            allPieces = concat [includesPieces, forwardDeclPieces, typedefPieces, classesCodePieces, functionsPieces]

            collectCodePieces fn = Data.List.intercalate "\n\n/****************/\n\n" (map fn allPieces)
            headerCode = collectCodePieces fst
            bodyCode = collectCodePieces snd
        in (headerCode, bodyCode)

standardSystemIncludes :: [CppInclude]
standardSystemIncludes = map CppSystemInclude ["memory", "vector", "string"]

translateToCppName :: Name -> String 
translateToCppName name = nameBase name

generateRootClassWrapper :: Dec -> CppClass
generateRootClassWrapper (DataD cxt name tyVars cons names) = 
    CppClass (translateToCppName name) [] [] []

isValueTypeInfo :: Info -> Q Bool
isValueTypeInfo (TyConI (TySynD name vars t)) = isValueType t
isValueTypeInfo _ = return False

isValueType :: Type -> Q Bool
isValueType (ConT name) | (elem name [''Int]) = return True
isValueType (ConT name) = do
    info <- reify name
    isValueTypeInfo info
isValueType _ = return False

--data WhatTheTypeIs = Primitive | Pointer | Maybe | List

--whatTypeIs :: Type -> WhatTheTypeIs
--whatTypeIs t@(ConT name) = do
--    let nb = nameBase name
--    byValue <- isValueType t
--    return $ 
--        if if byValue then nb
--        else "std::shared_ptr<" ++ nb ++ ">"


typeOfField :: Type -> Q String
typeOfField t@(ConT name) = do
    let nb = nameBase name
    byValue <- isValueType t
    return $ 
        if name == ''String then "std::string" 
        else if name == ''Int then "int"
        else if byValue then nb
        else "std::shared_ptr<" ++ nb ++ ">"

typeOfField (AppT (ConT base) nested) | (base == ''Maybe) = do
    nestedName <- typeOfField nested
    isNestedValue <- isValueType nested
    return $ if isNestedValue then "boost::optional<" ++ nestedName ++ ">" else nestedName

typeOfField (AppT ListT (nested)) = do
    nestedType <- typeOfField nested
    return $ printf "std::vector<%s>" $ nestedType
--typeOfField (AppT ConT (maybe)) = printf "boost::optional<%s>" $ typeOfField nested
typeOfField t = return $ "[" ++ show t ++ "]"

emptyQParts :: Q CppParts
emptyQParts = return $ CppParts [] [] []  [] []

processField :: THS.VarStrictType -> Q CppField
processField field@(name, _, t) = do
    filedType <- typeOfField t
    return $ CppField (translateToCppName name) filedType
processField arg = trace ("FIXME: Field for " ++ show arg) (return $ CppField "__" "--")

processConstructor :: Con -> Name -> Q CppClass
processConstructor con@(RecC cname fields) base = 
    do
        let baseCppName = translateToCppName base
            derCppName = baseCppName ++ "_" ++ translateToCppName cname
        cppFields <- mapM processField fields
        return $ CppClass derCppName cppFields [] [CppDerive baseCppName False Public]
processConstructor arg name = trace ("FIXME: Con for " ++ show arg) (return $ CppClass "" [] [] [])

generateCppWrapperHlp :: Dec -> Q CppParts
-- generateCppWrapperHlp arg | trace ("generateCppWrapperHlp: " ++ show arg) False = undefined
generateCppWrapperHlp dec@(DataD cxt name tyVars cons names) = 
    do
        let baseClass = generateRootClassWrapper dec
        derClasses <- sequence $ processConstructor <$> cons <*> [name]
        -- derClasses = processConstructor <$> cons <*> [name]
        let classes = baseClass : derClasses
            functions = []
            forwardDecs = map (\c -> CppForwardDeclClass $ className c) classes
        return (CppParts standardSystemIncludes forwardDecs [] classes functions)
generateCppWrapperHlp tysyn@(TySynD name tyvars rhstype) = do
    baseTName <- typeOfField rhstype
    let tf = CppTypedef (nameBase name) baseTName
    return $ CppParts [] [] [tf] [] []
generateCppWrapperHlp arg = trace ("FIXME: generateCppWrapperHlp for " ++ show arg) emptyQParts

--generateSingleWrapperInfo

generateSingleWrapper :: Name -> Q CppParts
generateSingleWrapper arg | trace ("generateSingleWrapper: " ++ show arg) False = undefined
generateSingleWrapper name = do
    nameInfo <- reify name
    let bb = case nameInfo of
            (TyConI dec) -> generateCppWrapperHlp dec
            _ -> trace ("ignoring entry " ++ show name) emptyQParts
    bb
generateSingleWrapper _ = undefined

joinParts :: [CppParts] -> CppParts
joinParts parts = 
    CppParts (concat $ map includes parts) (concat $ map forwardDecls parts) (concat $ map typedefs parts) (concat $ map classes parts) (concat $ map functions parts)

generateWrappers :: [Name] -> Q CppParts
generateWrappers names = do
    let partsWithQ = map generateSingleWrapper names
    parts <- sequence partsWithQ
    return $ joinParts parts

generateWrapperWithDeps :: Name -> Q CppParts
generateWrapperWithDeps name = do
    relevantNames <- collectDependencies name
    generateWrappers relevantNames


formatCppWrapper :: Name -> Q CppFormattedCode
formatCppWrapper arg | trace ("formatCppWrapper: " ++ show arg) False = undefined
formatCppWrapper name = do
    parts <- generateWrapperWithDeps name
    return $ formatCpp parts

foo :: Show a => a -> a
foo a | trace ("zzzz ") False = undefined
foo a = a


builtInTypes = [''Maybe, ''String, ''Int]

class TypesDependencies a where
    symbolDependencies :: a -> Set Name

instance TypesDependencies Type where
    --symbolDependencies t | trace ("Type: " ++ show t) False = undefined

    -- Maybe and String are handled as a special-case
    symbolDependencies (ConT name) | (elem name builtInTypes) = Set.empty
    symbolDependencies contype@(ConT name) = Set.singleton name
    symbolDependencies apptype@(AppT ListT nested) = symbolDependencies nested
    symbolDependencies apptype@(AppT base nested) = symbolDependencies [base, nested]
    symbolDependencies vartype@(VarT n) = Set.empty
    symbolDependencies t = trace ("FIXME not handled type: " ++ show t) Set.empty

instance (TypesDependencies a, Show a) => TypesDependencies [a] where
    --symbolDependencies t | trace ("list: " ++ show t) False = undefined
    symbolDependencies listToProcess =
        let listOfSets = (map symbolDependencies listToProcess)::[Set Name]
        in Set.unions listOfSets

instance TypesDependencies (Strict, Type) where
    symbolDependencies (_, t) = symbolDependencies t

instance TypesDependencies Con where
    --symbolDependencies t | trace ("Con: " ++ show t) False = undefined
    symbolDependencies (RecC name fields) = symbolDependencies fields
    symbolDependencies (NormalC name fields) = symbolDependencies fields
    symbolDependencies t = trace ("FIXME not handled Con: " ++ show t) (errorWithStackTrace) []

instance TypesDependencies VarStrictType where
    --symbolDependencies t | trace ("Field: " ++ show t) False = undefined
    symbolDependencies (_, _, t) = symbolDependencies t

instance TypesDependencies Info where
--    symbolDependencies t | trace ("Info: " ++ show t) False = undefined
    symbolDependencies (TyConI (DataD _ n _ cons _)) = symbolDependencies cons
    symbolDependencies (TyConI (TySynD n _ t)) = symbolDependencies t
    symbolDependencies arg = trace ("FIXME not handled Info: " ++ show arg) (Set.empty)

blah :: Name -> StateT [Name] Q [Name]
blah name = do
    nameInfo <- lift $ reify name
    return []

additionalDependencies :: Set Name -> Info -> Set Name
additionalDependencies alreadyKnownDeps queriedInfo = 
    let allDepsOfInfo = symbolDependencies queriedInfo
        newDeps = Set.difference alreadyKnownDeps allDepsOfInfo
    in newDeps

collectDirectDependencies :: Name -> Q [Name]
-- collectDirectDependencies name | trace ("collectDirectDependencies " ++ show name) False = undefined
collectDirectDependencies name = do
    nameInfo <- reify name
    let namesSet = symbolDependencies nameInfo
    return $ Set.elems namesSet
    -- evalStateT (blah name) []

naiveBfs :: [Name] -> [Name] -> Q [Name]
-- naiveBfs q d | trace (show q ++ "\n\n" ++ show d) False = undefined
naiveBfs [] discovered = return discovered
naiveBfs queue discovered = do
    let vertex = head queue
    neighbours <- collectDirectDependencies vertex
    let neighboursToAdd = filter (flip notElem discovered) neighbours
    let newQueue = tail queue ++ neighboursToAdd
    let newDiscovered = discovered ++ neighboursToAdd
    naiveBfs newQueue newDiscovered

collectDependencies :: Name -> Q [Name]
collectDependencies name = do
    let queue = [name]
    let discovered = [name]
    naiveBfs queue discovered

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

    dependencies <- collectDependencies name
    runIO (putStrLn $ printf "Found %d dependencies: %s" (length dependencies) (show dependencies))
    (header,body) <- formatCppWrapper name


    runIO (writeFile headerName header)
    runIO (writeFile cppName body)

    runIO (writeFile cppName (printf "#include \"%s\"" headerName))
    [|  return () |]

