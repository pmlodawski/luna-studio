---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Luna.Data.AST.Pat where

import           Control.Applicative
import           Flowbox.Generics.Deriving.QShow
import qualified Flowbox.Luna.Data.AST.Lit       as Lit
import           Flowbox.Luna.Data.AST.Type      (Type)
import           Flowbox.Luna.Data.AST.Utils     (ID)
import           Flowbox.Prelude                 hiding (Traversal, drop, id)
import           GHC.Generics

type Lit = Lit.Lit

data Pat = Var             { _id :: ID, _name      :: String                          }
         | Lit             { _id :: ID, _value     :: Lit                             }
         | Tuple           { _id :: ID, _items     :: [Pat]                           }
         | Con             { _id :: ID, _name      :: String                          }
         | App             { _id :: ID, _src       :: Pat       , _args      :: [Pat] }
         | Typed           { _id :: ID, _pat       :: Pat       , _cls       :: Type  }
         | Wildcard        { _id :: ID                                                }
         deriving (Show, Eq, Generic)

instance QShow Pat
makeLenses (''Pat)


type Traversal m = (Functor m, Applicative m, Monad m)

traverseM :: Traversal m => (Pat -> m Pat) -> (Type -> m Type) -> (Lit -> m Lit) -> Pat -> m Pat
traverseM fpat ftype flit p = case p of
    Lit        id' val'                      -> Lit   id' <$> flit val'
    Tuple      id' items'                    -> Tuple id' <$> fpatMap items'
    App        id' src' args'                -> App   id' <$> fpat src' <*> fpatMap args'
    Typed      id' pat' cls'                 -> Typed id' <$> fpat pat' <*> ftype cls'
    Var        {}                            -> pure p
    Con        {}                            -> pure p
    Wildcard   {}                            -> pure p
    where fpatMap = mapM fpat

traverseM_ :: Traversal m => (Pat -> m c) -> (Type -> m b) -> (Lit -> m d) -> Pat -> m ()
traverseM_ fpat ftype flit p = case p of
    Lit        _  val'                       -> drop <* flit val'
    Tuple      _  items'                     -> drop <* fpatMap items'
    App        _  src' args'                 -> drop <* fpat src' <* fpatMap args'
    Typed      _  pat' cls'                  -> drop <* fpat pat' <* ftype cls'
    Var        {}                            -> drop
    Con        {}                            -> drop
    Wildcard   {}                            -> drop
    where drop    = pure ()
          fpatMap = mapM_ fpat


traverseM' :: Traversal m => (Pat -> m Pat) -> Pat -> m Pat
traverseM' fpat p = traverseM fpat pure pure p


traverseM'_ :: Traversal m => (Pat -> m ()) -> Pat -> m ()
traverseM'_ fpat p = traverseM_ fpat pure pure p


