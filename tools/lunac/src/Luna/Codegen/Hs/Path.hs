---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.Path (
    module Luna.Network.Path.Path,
    toModulePath,
    toModuleName,
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
    
)where

import           Luna.Network.Path.Path 
import           Data.Char                      (isLower)



--toModulePath path = case path of
--    Path []                       -> Path []
--    Path (segment@(preffix:_):xs) -> npath where
--                                         name = if isLower preffix
--                                            then "U'" ++ segment
--                                            else segment
--                                         npath = prepend name $ toModulePath (Path xs)

toModulePath :: Path -> Path
toModulePath path = Path $ map toModuleName (segments path)

toModuleName :: String -> String
toModuleName name@(preffix:_) = if isLower preffix
        then "U'" ++ name
        else name
                                  
    
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

--mkGSName :: String -> String
--mkGSName name = name ++ "'GS"


mkGetter :: String -> String
mkGetter name = name ++ "'getter"


mkSetter :: String -> String
mkSetter name = name ++ "'setter"


mkTHPointer :: String -> String
mkTHPointer name = "'" ++ name


inputs :: String
inputs = "inputs'"


outputs :: String
outputs = "outputs'"

indent :: String
indent = replicate 4 ' '

mkIndent :: Int -> String
mkIndent i = concat $ replicate i indent