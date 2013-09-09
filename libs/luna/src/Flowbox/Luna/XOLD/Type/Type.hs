---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.XOLD.Type.Type(
    Type(..),

    noFields,
    noInputs,
    noOutputs,
    noParams,
    mkClass,
    mkModule,
    mkFunction,
) where

import           Flowbox.Prelude   

data Type = Undefined
          | TypeName     {name   :: String}
          | Module       {name   :: String,                     fields :: [Type] }
          | Class        {name   :: String, params :: [String], fields :: [Type]}
          | Function     {name   :: String, inputs ::  Type , outputs :: Type}
          | Interface    {fields :: [Type], methods :: [Type]}
          | Tuple        {items  :: [Type]}
          | Named        {name   :: String, cls :: Type}
          deriving (Show)

noFields :: [Type]
noFields = []

noInputs :: Type
noInputs = Tuple []

noOutputs :: Type
noOutputs = Tuple []

noParams :: [String]
noParams = []

mkClass :: String -> Type
mkClass name' = Class name' noParams noFields

mkModule :: String -> Type
mkModule name' = Module name' noFields

mkFunction :: String -> Type
mkFunction name' = Function name' noInputs noOutputs
