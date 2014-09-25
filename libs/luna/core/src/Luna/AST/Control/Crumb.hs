---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.AST.Control.Crumb where

import Flowbox.Prelude
import Luna.AST.Common (ID)



type Breadcrumbs = [Crumb]


data Crumb = Lambda   { _id :: ID     }
           | Function { _name :: String
                      , _path :: [String]
                      }
           | Class    { _name :: String }
           | Module   { _name :: String }
           deriving (Show, Ord, Eq)


makeLenses(''Crumb)


isFunction :: Crumb -> Bool
isFunction (Function {}) = True
isFunction _             = False


isClass :: Crumb -> Bool
isClass (Class {}) = True
isClass _          = False


isModule :: Crumb -> Bool
isModule (Module {}) = True
isModule _           = False


isLambda :: Crumb -> Bool
isLambda (Lambda {}) = True
isLambda _           = False
