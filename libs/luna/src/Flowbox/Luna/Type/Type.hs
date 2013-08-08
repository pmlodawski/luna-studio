---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Type.Type(
Type(..),

noInputs,
noOutputs,
noParams,
mkFunction,
mkModule
) where

data Type = Undefined
          | TypeVariable {name   :: String}
          | Class        {name   :: String, typeparams :: [String], params :: [Type]}
          | Function     {name   :: String, inputs ::  Type, outputs :: Type}
          | Tuple        {items  :: [Type]}
          | List         {item   ::  Type }
          | Interface    {fields :: [Type], methods :: [Type]}
          | Module       {name   :: String}
          | Named        {name   :: String, cls :: Type}
          deriving (Show)

noInputs :: Type
noInputs = Tuple []

noOutputs :: Type
noOutputs = Tuple []

noParams :: [Type]
noParams = []

mkModule :: String -> Type
mkModule name' = Module name'

mkFunction :: String -> Type
mkFunction name' = Function name' noInputs noOutputs