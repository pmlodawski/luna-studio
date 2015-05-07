---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Control.Crumb where

import           Flowbox.Prelude
import qualified Luna.Syntax.AST       as AST
import qualified Luna.Syntax.Decl      as Decl
import           Luna.Syntax.Name      (TNameP)
import           Luna.Syntax.Name.Path (NamePath, QualPath)



type Breadcrumbs = [Crumb]


data Crumb = Module   { _mpath :: QualPath }
           | Class    { _className :: TNameP }
           | Function { _functionPath :: Decl.Path
                      , _functionName :: NamePath }
           | Lambda   { _id :: AST.ID }
           deriving (Show, Ord, Eq)


makeLenses ''Crumb


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
