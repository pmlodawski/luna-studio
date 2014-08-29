---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.AST.Control.Crumb where

import Flowbox.Prelude



type Breadcrumbs = [Crumb]


data Crumb = Function { name :: String
                      , path :: [String]
                      }
           | Class    { name :: String }
           | Module   { name :: String }
           deriving (Show, Ord, Eq)
