---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Type.Type(
Type(..),

noImports,
noInputs,
noOutputs,
noParams,

makePackage
) where


data Type = Undefined
          | TypeVariable {name   :: String}
          | Class        {name   :: String, params :: [Type]}
          | Function     {name   :: String, inputs :: [Type], outputs :: [Type]}
          | Tuple        {items  :: [Type]}
          | List         {item   ::  Type }
          | Interface    {fields :: [Type], methods :: [Type]}
          | Package      {name   :: String, imports :: [String]}
          | Named        {name   :: String, cls :: Type}
          deriving (Show)

noImports :: [String]
noImports = []

noInputs :: [String]
noInputs = []

noOutputs :: [String]
noOutputs = []

noParams :: [String]
noParams = []

makePackage :: String -> Type
makePackage name' = Package name' noImports