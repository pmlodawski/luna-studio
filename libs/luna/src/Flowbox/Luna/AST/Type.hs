---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, ConstraintKinds #-}

module Flowbox.Luna.AST.Type where

import           Flowbox.Prelude                 hiding (id)
import           Flowbox.Generics.Deriving.QShow   
import           Flowbox.Luna.AST.Utils            (ID)
import           GHC.Generics                      
import           Control.Applicative               

data Type = Unknown { id :: ID                                           }
          | Var     { id :: ID, name     :: String                       }
          | Tuple   { id :: ID, items    :: [Type]                       }
          | Class   { id :: ID, name     :: String , params  :: [String] }
          | Module  { id :: ID, name     :: String                       }
          | Lambda  { id :: ID, inputs   :: [Type] , output  :: Type     }
          | Cons    { id :: ID, name     :: String                       }
          | App     { id :: ID, src      :: Type   , args    :: [Type]   }
          -- | List
          -- | Map
          deriving (Show, Eq, Generic)


instance QShow Type



type Traversal m = (Functor m, Applicative m, Monad m)

traverseM :: Traversal m => (Type -> m Type) -> Type -> m Type
traverseM ftype t = case t of
    Tuple      id items                      -> Tuple  id <$> ftypeMap items
    Lambda     id inputs output              -> Lambda id <$> ftypeMap inputs <*> ftype output
    App        id src args                   -> App    id <$> ftype    src    <*> ftypeMap args
    Var        {}                            -> pure t
    Class      {}                            -> pure t
    Module     {}                            -> pure t
    Cons       {}                            -> pure t
    _                                        -> fail "Unexpected type"
    where ftypeMap = mapM ftype

traverseM_ :: Traversal m => (Type -> m b) -> Type -> m ()
traverseM_ ftype t = case t of
    Tuple      id items                      -> drop <* ftypeMap items
    Lambda     id inputs output              -> drop <* ftypeMap inputs <* ftype output
    App        id src args                   -> drop <* ftype    src    <* ftypeMap args
    Var        {}                            -> drop
    Class      {}                            -> drop
    Module     {}                            -> drop
    Cons       {}                            -> drop
    _                                        -> fail "Unexpected type"
    where drop     = pure ()
          ftypeMap = mapM_ ftype


