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

mkPackage
) where

import           Luna.Network.Path.Import          (Import(..))


data Type = Undefined
          | TypeVariable {name   :: String}
          | Class        {name   :: String, params :: [Type]}
          | Function     {name   :: String, inputs :: [Type], outputs :: [Type]}
          | Tuple        {items  :: [Type]}
          | List         {item   ::  Type }
          | Interface    {fields :: [Type], methods :: [Type]}
          | Package      {name   :: String, imports :: [Import]}
          | Named        {name   :: String, cls :: Type}
          deriving (Show)

noImports :: [Import]
noImports = []

noInputs :: [Type]
noInputs = []

noOutputs :: [Type]
noOutputs = []

noParams :: [Type]
noParams = []

mkPackage :: String -> Type
mkPackage name' = Package name' noImports

mkFunction :: String -> Type
mkFunction name' = Function name' noInputs noOutputs