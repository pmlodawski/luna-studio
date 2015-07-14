{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Generator.Cython.CyGen where

import qualified Generator.Generator     as G
import qualified Generator.Cython.Common as CyCommon
import           Generator.Cython.CyAST
import qualified Generator.Cython.Utils  as CyUtils


import           Control.Lens
import           Data.List                  (intercalate)
import           Data.Monoid                ((<>))
import           Data.String.Utils          (split, replace)
import           System.FilePath            ((</>))
--import           Text.Show.Pretty           (ppShow)

import           Language.Haskell.TH

------------------------------------------------------------
-- Classes
------------------------------------------------------------

class CythonFormattable a where
    formatCython :: String -> Int -> a -> String   -- string -> header name, int -> indent

------------------------------------------------------------
-- Converting C++ structs into Cython ones
------------------------------------------------------------

cythonizeCppParts :: G.CppParts -> CyProgram
cythonizeCppParts cppParts = CyProgram classes statWraps functions
    where classes   = map cythonizeCppClass $ cppParts ^. G.classes
          functions = map cythonizeCppFunc  $ cppParts ^. G.functions
          statWraps = concatMap extractStatic classes


cythonizeCppClass :: G.CppClass -> CyClass
cythonizeCppClass cppClass = CyClass name fields methods conss bases enums templatePars
    where name         = cppClass ^. G.className
          conss        = createCyConstructors     cppClass
          fields       = cppClass ^. G.classFields         & map cythonizeCppField
          methods      = cppClass ^. G.classMethods        & filter (not . isConstructor)
                                                           & filter (if isBaseClass cppClass then const True
                                                                                             else not . isVirtual)
                                                           & map cythonizeCppMethod
          bases        = cppClass ^. G.classBases          & map G._baseName
          enums        = cppClass ^. G.classEnums          & map cythonizeCppEnum
          templatePars = cppClass ^. G.classTemplateParams & map cythonizeCppType


createCyConstructors :: G.CppClass -> [CyCons]
createCyConstructors cppClass = cppClass ^. G.classMethods & filter isConstructor & map cythonizeCppCons


cythonizeCppCons :: G.CppMethod -> CyCons
cythonizeCppCons cppMethod = CyCons cName cArgs
    where cName = cppMethod ^. G.function ^. G.name
          cArgs = cppMethod ^. G.function ^. G.args & map cythonizeCppArg


cythonizeCppFunc :: G.CppFunction -> CyFunction
cythonizeCppFunc cppFunc = CyFunction funcName funcArgs funcRetType
    where funcName    = cppFunc ^. G.name
          funcArgs    = map cythonizeCppArg $ cppFunc ^. G.args
          funcRetType = cythonizeCppType    $ cppFunc ^. G.returnType


extractStatic :: CyClass -> [CyStatWrap]
extractStatic cyClass = cyClass ^. classMethods & filter _methodStatic
                                                & map (\mthd -> CyStatWrap mthd
                                                                           (cyClass ^. className)
                                                                           (cyClass ^. classTemplatePars))


cythonizeCppMethod :: G.CppMethod -> CyMethod
cythonizeCppMethod cppMethod = CyMethod mName mArgs mRetType mStatic
    where mFunc    = cppMethod ^. G.function
          mName    = mFunc ^. G.name
          mArgs    = map cythonizeCppArg $ mFunc ^. G.args
          mRetType = cythonizeCppType $ mFunc ^. G.returnType
          mStatic  = G.isStatic cppMethod


cythonizeCppField :: G.CppField -> CyField
cythonizeCppField cppField = CyField (cppField ^. G.fieldName)
                                     (cythonizeCppType $ cppField ^. G.fieldType)


cythonizeCppArg :: G.CppArg -> CyArg
cythonizeCppArg cppArg = CyArg (cppArg ^. G.argName) (cythonizeCppType $ cppArg ^. G.argType)


cythonizeCppType :: String -> CyType
cythonizeCppType cppType = CyType . cythonizeType $ cppType --templs
        --where typeNoNS        = stripNamespaceQual cppType
        --   typeNoBT        = if (not $ null typeNoNS)  && (last typeNoNS == '>') then init typeNoNS else typeNoNS
        --   typeSplit       = split "<" typeNoBT
        --   (tBase, templs) = case typeSplit of []          -> ([], [])
        --                                       [t]         -> (t, [])
        --                                       (t:tmpls:_) -> (t, split ", " tmpls)


cythonizeCppEnum :: G.CppEnum -> CyEnum
cythonizeCppEnum cppEnum = CyEnum (cppEnum ^. G.enumName) (cppEnum ^. G.enumElems)


-------------------------------------------------------------------
-- Converting Cython structs to literal code
-------------------------------------------------------------------

instance CythonFormattable CyProgram where
    formatCython headerName ind cyProgram = intercalate "\n\n\n" [classCode, wrappersCode, functionCode]
        where classCode    = cyProgram ^. cyClasses   & map (formatCython headerName ind) & intercalate "\n\n"
              wrappersCode = cyProgram ^. cyStatWraps & map (formatCython headerName ind) & intercalate "\n\n"
              functionCode = cyProgram ^. cyFunctions & map (formatCython headerName ind) & intercalate "\n\n"


instance CythonFormattable CyClass where
    formatCython headerName ind cyClass = externDef <> "\n" <> turboCalate "\n\n" [enums, classDef]
        where externDef      = mkIndent ind <> formatExternDef headerName Nothing
              classDef       = turboCalate "\n" [classHeader, conss, fields, methods]
              classHeader    = mkIndent (ind+1) <> "cdef cppclass " <> cyClass ^. className
                                                <> templateParams   <> bases <> ":"
              templateParams = if null $ cyClass ^. classTemplatePars
                  then ""
                  else "[" <> (cyClass ^. classTemplatePars & map (formatCython headerName 0) & intercalate ", ") <>  "]"
              bases          = if null $ cyClass ^. classBases
                  then ""
                  else "(" <> (cyClass ^. classBases & map (<> templateParams) & intercalate ", ") <>  ")"
              methods        = cyClass ^. classMethods & filter (not . _methodStatic)
                                                       & map (formatCython headerName (ind+2)) & intercalate "\n"
              conss          = cyClass ^. classConss   & map (formatCython headerName (ind+2)) & intercalate "\n"
              fields         = cyClass ^. classFields  & map (formatCython headerName (ind+2)) & intercalate "\n"
              enums          = cyClass ^. classEnums   & map (formatCython headerName (ind+1)) & intercalate "\n\n"


instance CythonFormattable CyCons where
    formatCython headerName ind cyCons = mkIndent ind <> cyCons ^. consName <> args <> " except +"
        where args = "(" <> (cyCons ^. consArgs & map (formatCython headerName 0) & intercalate ", ") <> ")"


instance CythonFormattable CyMethod where
    formatCython headerName ind cyMethod = mkIndent ind <> mType <> " " <> cyMethod ^. methodName <> mArgs
        where mArgs = "(" <> (cyMethod ^. methodArgs & map (formatCython headerName 0) & intercalate ", ") <> ")"
              mType = formatCython headerName 0 $ cyMethod ^. methodRetType


-- cdef extern from "Bum.h":
--     shared_ptr[Generator_Data_Bum_Bum[a_1627498547,b_1627498548]] Generator_Data_Bum_Bum_deserializeFrom[a_1627498547, b_1627498548](Input & input)
instance CythonFormattable CyStatWrap where
    formatCython headerName ind cyStatWrap = externDef <> "\n" <> methodSig
        where externDef  = mkIndent ind <> formatExternDef headerName Nothing
              methodSig  = mkIndent (ind+1) <> mRetType <> " " <> mName <> mTemplates <> mArgs
              mRetType   = cyStatWrap ^. statMethod ^. methodRetType & formatCython headerName 0
              mName      = cyStatWrap ^. statClassName <> "_" <> cyStatWrap ^. statMethod ^. methodName  -- <> mTemplates
              mTemplates = formatTemplates $ cyStatWrap ^. statTemplates
              mArgs      = "(" <> (cyStatWrap ^. statMethod ^. methodArgs & map (formatCython headerName 0)
                                                                          & intercalate ", ") <> ")"


instance CythonFormattable CyField where
    formatCython headerName ind cyField = mkIndent ind <> fType <> " " <> fName
        where fType = formatCython headerName 0 $ cyField ^. fieldType
              fName = cyField ^. fieldName


instance CythonFormattable CyFunction where
    formatCython headerName ind cyFunction = "undefined"


instance CythonFormattable CyArg where
    formatCython headerName ind cyArg = (cyArg ^. argType & formatCython headerName 0) <> " " <> cyArg ^. argName


instance CythonFormattable CyType where
    formatCython headerName ind cyType = cyType ^. typeName   -- TODO (!!)


instance CythonFormattable CyEnum where
    formatCython headerName ind cyEnum = mkIndent ind <> "cdef enum " <> cyEnum ^. enumName <> ":\n" <> fields
        where fields = cyEnum ^. enumElems & map (mkIndent (ind+1) <>) & intercalate "\n"


------------------------------------------------------------
-- Block formatting
------------------------------------------------------------
indent :: String
indent = "    "


mkIndent :: Int -> String
mkIndent count = concat $ replicate count indent


-----------------------------------------------------------
-- Utils
-----------------------------------------------------------
turboCalate :: [a] -> [[a]] -> [a]
turboCalate sep = intercalate sep . filter (not . null)


-- cython's "cdef extern from <header_name> namespace <namespace>:"
formatExternDef :: String -> Maybe String -> String
formatExternDef headerFile namespace = "cdef extern from \"" <> headerFile <> "\"" <> nmspc <> ":"
    where nmspc = case namespace of (Just n) -> " namespace \"" <> n <> "\""
                                    Nothing  -> ""


formatTemplates :: [CyType] -> String
formatTemplates cyTypes = if null cyTypes
    then ""
    else "[" <> (cyTypes & map _typeName & intercalate ", ") <>  "]"

isStatic :: G.CppMethod -> Bool
isStatic method = case method ^. G.storage of G.Static -> True
                                              _        -> False


isConstructor :: G.CppMethod -> Bool
isConstructor method = null $ method ^. G.function ^. G.returnType


isVirtual :: G.CppMethod -> Bool
isVirtual method = case method ^. G.storage of G.Virtual -> True; _ -> False


isBaseClass :: G.CppClass -> Bool
isBaseClass cppClass = null $ cppClass ^. G.classBases


stripNamespaceQual :: String -> String
stripNamespaceQual name = if not $ null splitted then last splitted
                                                 else ""
    where splitted = split "::" name


cythonizeTemplate ::  String -> String
cythonizeTemplate = replace "<" "[" . replace ">" "]"


cythonizeType :: String -> String
cythonizeType = cythonizeTemplate . stripNamespaceQual


------------------------------------------------------------------------------
-- Generating bindings with TH
------------------------------------------------------------------------------
generateCython :: Name -> FilePath -> DecsQ
generateCython name outputDir = do
    cppParts <- G.generateCythonInfo name
    let tName      = nameBase name
        headerName = tName <> ".h"
        cyFileName = "cy" <> tName <> ".pxd"
        cyProg     = cythonizeCppParts $ head cppParts
        formatted  = formatCython headerName 0 cyProg
        cyCode     = formatted <> "\n\n\n"

    --runIO $ writeFile "test/cyProg" (ppShow cyProg)
    runIO $ writeFile (outputDir </> G.nameToDir name </> cyFileName) cyCode

    return []


generateBindings :: Name -> FilePath -> DecsQ
generateBindings name outputDir = do
    _ <- G.generateCpp  name outputDir
    _ <- generateCython name outputDir

    return []


collectBindings :: String -> FilePath -> DecsQ
collectBindings modName outputDir = do
    let nameDir  = outputDir </> CyUtils.modulePath modName
        fileName = nameDir <> ".pxd"

    pxds <- runIO $ CyUtils.getPxdsForDir nameDir

    runIO $ CyUtils.safeRemoveFile fileName
    let pxdsFullP = map (nameDir </>) pxds

    runIO $ appendFile fileName CyCommon.cyHeader
    runIO $ mapM_ (CyUtils.appendFileFromDisk fileName) pxdsFullP

    return []
