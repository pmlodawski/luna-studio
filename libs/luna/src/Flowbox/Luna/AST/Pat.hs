---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Luna.AST.Pat where

import           Flowbox.Prelude
import qualified Flowbox.Luna.AST.Lit      as Lit
import qualified Flowbox.Luna.AST.Type     as Type
import           Flowbox.Luna.AST.Type       (Type)
import           Flowbox.Generics.Deriving.QShow
import           GHC.Generics

type Lit = Lit.Lit

data Pat = Var             { name      :: String                  }
         | Lit             { value     :: Lit                     }
         | Tuple           { items     :: [Pat]                   }
         | Cons            { name      :: String  , args :: [Pat] }
         | Typed           { cls       :: Type    , pat  :: Pat   }
         | CallConstructor { args      :: [Pat]                   }
         | Wildcard
         deriving (Show, Eq, Generic)

instance QShow Pat



callConstructor :: Pat -> Pat -> Pat
callConstructor src' arg' = case src' of
    call @ CallConstructor{} -> call { args = args call ++ [arg'] }
    _                        -> CallConstructor $ src':[arg']


aftermatch :: String -> Pat -> Pat
aftermatch src' dst' = case dst' of
    CallConstructor args' -> Cons src' args'
    _                     -> Cons src' [dst']