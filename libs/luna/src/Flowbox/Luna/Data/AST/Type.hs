---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, ConstraintKinds #-}

module Flowbox.Luna.Data.AST.Type where

import           Flowbox.Prelude                 hiding (id, drop, Traversal)
import           Flowbox.Generics.Deriving.QShow   
import           Flowbox.Luna.Data.AST.Utils       (ID)
import           GHC.Generics                      
import           Control.Applicative               

data Type = Unknown { _id :: ID                                               }
          | Var     { _id :: ID, _name     :: String                          }
          | Tuple   { _id :: ID, _items    :: [Type]                          }
          | Class   { _id :: ID, _name     :: String   , _params  :: [String] }
          | Module  { _id :: ID, _path     :: [String]                        }
          | Lambda  { _id :: ID, _inputs   :: [Type]   , _output  :: Type     }
          | Con     { _id :: ID, _segments :: [String]                        }
          | App     { _id :: ID, _src      :: Type     , _args    :: [Type]   }
          deriving (Show, Eq, Generic)


instance QShow Type
makeLenses (''Type)

type Traversal m = (Functor m, Applicative m, Monad m)

traverseM :: Traversal m => (Type -> m Type) -> Type -> m Type
traverseM ftype t = case t of
    Tuple      id' items'                    -> Tuple  id' <$> ftypeMap items'
    Lambda     id' inputs' output'           -> Lambda id' <$> ftypeMap inputs' <*> ftype output'
    App        id' src' args'                -> App    id' <$> ftype    src'    <*> ftypeMap args'
    Var        {}                            -> pure t
    Class      {}                            -> pure t
    Module     {}                            -> pure t
    Con        {}                            -> pure t
    Unknown    {}                            -> pure t
    where ftypeMap = mapM ftype

traverseM_ :: Traversal m => (Type -> m b) -> Type -> m ()
traverseM_ ftype t = case t of
    Tuple      _  items'                     -> drop <* ftypeMap items'
    Lambda     _  inputs' output'            -> drop <* ftypeMap inputs' <* ftype output'
    App        _  src' args'                 -> drop <* ftype    src'    <* ftypeMap args'
    Var        {}                            -> drop
    Class      {}                            -> drop
    Module     {}                            -> drop
    Con        {}                            -> drop
    Unknown    {}                            -> drop
    where drop     = pure ()
          ftypeMap = mapM_ ftype


