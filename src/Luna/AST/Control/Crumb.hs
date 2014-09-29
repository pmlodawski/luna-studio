---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.AST.Control.Crumb where

import           Flowbox.Prelude
import qualified Luna.AST.Common as AST



type Breadcrumbs = [Crumb]


data Crumb = Module   { _name :: String }
           | Class    { _name :: String }
           | Function { _name :: String
                      , _path :: [String]
                      }
           | Lambda   { _id :: AST.ID }
           deriving (Show, Ord, Eq)


makeLenses(''Crumb)


isModule :: Crumb -> Bool
isModule (Module {}) = True
isModule _           = False


isClass :: Crumb -> Bool
isClass (Class {}) = True
isClass _          = False


isFunction :: Crumb -> Bool
isFunction (Function {}) = True
isFunction _             = False


isLambda :: Crumb -> Bool
isLambda (Lambda {}) = True
isLambda _           = False
