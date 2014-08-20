---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Pass.Transform.HAST.HASTGen.Path (
    module Flowbox.Data.Path,
    toModulePath,
    toModuleName,
    toFilePath,
    mkTemplateName,
    mkMonadName,
    mkLensName,
    mkFieldName,
    inputs,
    outputs,
    indent,
    mkGetter,
    mkSetter,
    mkIndent,
    mkTHPointer,
    mkClassName,
    toString,
    mkCommonImportName,
    mkFuncName
)where

import Data.Char         (isLower)
import Data.String.Utils (join)

import Flowbox.Data.Path
import Flowbox.Prelude   hiding (init, last, last, tail)



toModulePath :: Path -> Path
toModulePath path = Path $ map toModuleName (segments path)

toModuleName :: String -> String
toModuleName name = case name of
    ""          -> ""
    (preffix:_) -> if isLower preffix
                   then "U'" ++ name
                   else name

toFilePath :: Path -> Path
toFilePath path = append (last modpath ++ ".hs") (init modpath) where
    modpath = toModulePath path

mkTemplateName :: String -> String
mkTemplateName name = name ++ "'T"

mkMonadName :: String -> String
mkMonadName name = name ++ "''M"

mkLensName :: String -> String
mkLensName name = '_' : name

mkFieldName :: String -> String
mkFieldName name = name ++ "'F"

mkClassName :: String -> String
mkClassName name = "C''" ++ name

mkCommonImportName :: String -> String
mkCommonImportName = mkClassName . mkFuncName

mkFuncName :: String -> String
mkFuncName name = name ++ "'"

mkGetter :: String -> String
mkGetter name = name ++ "'getter"

mkSetter :: String -> String
mkSetter name = name ++ "'setter"

mkTHPointer :: String -> String
mkTHPointer name = "'" ++ name

inputs :: String
inputs = "inputs''"

outputs :: String
outputs = "outputs''"

indent :: String
indent = replicate 4 ' '

mkIndent :: Int -> String
mkIndent i = concat $ replicate i indent

toString :: Path -> String
toString path = join "." $ segments path
