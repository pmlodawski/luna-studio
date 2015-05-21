{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Generator.FFI where

-- import Generator.Expr (adjust, Point(..))
import Generator.Generator

import Data.Functor
import Debug.Trace
import Control.Monad

import Foreign.Storable (poke)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (peekArray, newArray)
import Data.ByteString.Lazy (ByteString, empty, pack, unpack)
import Language.Haskell.TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (VarStrictType)
import qualified Language.Haskell.TH.Syntax as THS
import qualified Data.Set as Set

import Control.Applicative ((<*>))
import Data.Binary (encode, decode)

import Data.Monoid ((<>))

import Data.Int   -- eg. Int64
import Data.Word   -- eg. Word8
import Data.Default

import Data.List (intercalate)
import Foreign.Ptr (Ptr)

import NeatInterpolation
import Text.Printf (printf)


--adjustFFI :: Int -> Ptr Word8 -> Ptr Int64 -> IO (Ptr Word8)
--adjustFFI inSize inPtr outSizePtr = do
    --inputBytes <- peekArray inSize inPtr
    --let inputBytestring = pack inputBytes
    --let outputBytestring = adjust' inputBytestring
    --let outputBytes = unpack outputBytestring
    --let outputBytesCount = fromIntegral $ length outputBytes :: Int64

    --poke outSizePtr outputBytesCount
    --outPtr <- newArray outputBytes
    --return outPtr

indent :: Int -> String -> String
indent level text = unlines $ ((<>) $ replicate level '\t') <$> (lines text)

generateCppWrapperBody :: String -> [String] -> String -> String
generateCppWrapperBody ffiName argNames retType = body where
    serializeCalls = intercalate "\n" $ printf "serialize(%s, out);" <$> argNames
    resultDecl = printf "%s result;" retType
    body = indent 1 $ [string|    
               std::ostringstream out;

               //////////////////////////////////////////////////////////////////////////
               $serializeCalls
               $resultDecl
               //////////////////////////////////////////////////////////////////////////

               auto argData = out.str();
               auto arglength = argData.size();
               auto argdata = argData.data();

               std::int64_t bufferSize = -1;

               auto resultptr = $ffiName(arglength, (void*)argdata, &bufferSize);
                
               imemstream memstream((char*)resultptr, bufferSize);

               deserialize(result, memstream);

               hsfree(resultptr);
               return result;
           |]

generateCppWrapper :: Name -> Name -> [Type] -> Type -> Q CppFunction
generateCppWrapper fname fnameFfi argsTypes retType = do
    retTypeCpp <- hsTypeToCppType retType TypeField
    argsTypesCpp <- sequence $ hsTypeToCppType <$> argsTypes <*> [TypeField]
    let argsNames = (printf "arg%d") <$> [0 .. length argsTypes - 1]
    let argsCpp = zipWith CppArg argsNames argsTypesCpp
    let functionBody = generateCppWrapperBody (nameBase fnameFfi) argsNames retTypeCpp
    return $ CppFunction (nameBase fname) retTypeCpp argsCpp functionBody

makeFunction :: Name -> Type -> [Pat] -> Exp -> [Dec] -> [Dec]
makeFunction fname ftype args bodyExp whereDecs = [prototype, definition] where
    prototype = SigD fname ftype
    definition = FunD fname [Clause args (NormalB bodyExp) whereDecs]

makeExportedFunction :: Name -> Type -> [Pat] -> Exp -> [Dec] -> [Dec]
makeExportedFunction fname ftype args bodyExp whereDecs = decs <> [export] where
    decs = makeFunction fname ftype args bodyExp whereDecs
    export = ForeignD $ ExportF CCall (nameBase fname) fname ftype

generateFFIWrapper :: Name -> Name -> Q [Dec]
generateFFIWrapper ffiFname bsWrapperFname  = do

    ptrWrapperType <- [t| Int -> Ptr Word8 -> Ptr Int64 -> IO (Ptr Word8) |]
    
    let a  = "Test"

    argInputSize <- newName "inputSize"
    argInputPtr <- newName "inputPtr"
    argOutResultSize <- newName "inputOutResultSize"
    let args = [argInputSize, argInputPtr, argOutResultSize]

    bsWrapperBody <- [e| do 
                            inputBytes <- peekArray $(varE argInputSize) $(varE argInputPtr)
                            let inputBytestring = pack inputBytes
                            let outputBytestring = $(varE bsWrapperFname) inputBytestring
                            let outputBytes = unpack outputBytestring
                            let outputBytesCount = fromIntegral $ length outputBytes :: Int64

                            poke $(varE argOutResultSize) outputBytesCount
                            outPtr <- newArray outputBytes
                            return outPtr|]

    return $ makeExportedFunction 
                        ffiFname 
                        ptrWrapperType 
                        (VarP <$> args)
                        bsWrapperBody
                        []

generateByteStringWrapper :: Name -> Name -> Int -> Q [Dec]
generateByteStringWrapper fname fnameBs argCount = do
    argNames <- replicateM argCount (newName "arg")
    let argPattern = tupP (varP <$> argNames)
    let callExpr = return $ callWithArgs fname argNames :: Q Exp


    bsWrapperType <- [t| ByteString -> ByteString |]
    bsWrapperArg <- newName "argBin"
    bsWrapperResult <- newName "resultBin"
    let bsWrapperBody = VarE bsWrapperResult
    bsWrapperWhereDecs <- [d| $(argPattern) = decode $(varE bsWrapperArg)
                              result = $(callExpr)
                              $(varP bsWrapperResult) = encode result|] 

    return $ makeFunction fnameBs bsWrapperType [VarP bsWrapperArg] bsWrapperBody bsWrapperWhereDecs


generateFFI :: Name -> Q (CppFunction, [Dec])
generateFFI fname = do

    n@(VarI fname ftype _ _) <- reify fname
    let argsTypes = argumentTypes ftype
    let n = nameBase fname
    let fnameFfi = mkName $ n ++ "_ffi"
    let fnameBs = mkName $ n ++ "'"

    binaryWrapperDecl <- generateByteStringWrapper fname fnameBs (length argsTypes)
    ------------------------------------------
    ffiWrapperDecl <- generateFFIWrapper fnameFfi fnameBs
    ------------------------------------------
    cppWrapper <- generateCppWrapper fname fnameFfi argsTypes $ returnedType ftype
    ------------------------------------------
    let decs = binaryWrapperDecl <> ffiWrapperDecl

    return $ (cppWrapper, decs)


hsfreeFunction :: Q [Dec]
hsfreeFunction = do
    ptr <- newName "ptr"
    ftype <- [t| Ptr Word8 -> IO () |]
    farg <- [p| $(varP ptr) |]
    fbody <- [e| free $(varE ptr) |]
    return $ makeExportedFunction
        (mkName "hsfree")
        ftype
        [farg]
        fbody
        []
                            
data MethodStuff = MethodStuff {
    cppMethod::CppMethod, 
    dependencies::[Name], 
    generatedDecls::[Dec]
}

generateDllStuff :: Name -> Q MethodStuff
generateDllStuff fname = do
    deps <- collectDependencies fname
    (cppFunction, decs) <- generateFFI $ fname
    let method = CppMethod cppFunction [] Static
    return $ MethodStuff method deps decs

runtimeCtor :: String -> CppMethod
runtimeCtor clsname = CppMethod fun [] Usual where
    body = indent 1 $ [string|     
        int argc = 1;
        char* argv[] = { "ghcDll", NULL }; // argv must end with NULL
        char** args = argv;
        hs_init(&argc, &args);
        |]
    fun = CppFunction clsname "" [] body


runtimeDtor :: String -> CppMethod
runtimeDtor clsname = CppMethod fun [] Virtual where
    body = indent 1 "hs_exit();"
    fun = CppFunction ("~"<>clsname) "" [] body

generateDllInterface :: [Name] -> FilePath -> Q [Dec]
generateDllInterface fnames outputDir = do
    module_name <- (loc_module <$> location)

    methodsInfo <- sequence $ generateDllStuff <$> fnames
    let deps = concat $ dependencies <$> methodsInfo
    let decs = concat $ generatedDecls <$> methodsInfo

    depParts <- cppDependenciesParts deps

    let clsname = ("HaskellDll"<>module_name)
    let cppMethods = (cppMethod <$> methodsInfo) <> [runtimeCtor clsname, runtimeDtor clsname]
    let cls = CppClass clsname [] cppMethods [] [] []

    let stubIncludePath = printf "../../hs/dist/build/%s_stub.h" module_name
    let includes = (def, Set.fromList [CppSystemInclude "sstream", CppLocalInclude stubIncludePath])

    let globalVar = CppGlobalVariable "hsdll" clsname

    writeFilePair outputDir "DllApi" $ joinParts [(CppParts includes def def [cls] [] [globalVar]), depParts]
    generateCppList deps outputDir


    hsfreeDecs <- hsfreeFunction
    return (decs <> hsfreeDecs)





--main = do
--    return $(generateFFI "")
--    let foo = adjust "" $ [Point 2 4]
--    putStrLn "fff"
---- main = generateFFI ''adjust "ttt"