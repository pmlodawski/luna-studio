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

data Type = Unknown
          | Var    { id :: ID, name     :: String                       }
          | Tuple  { id :: ID, items    :: [Type]                       }
          | Class  { id :: ID, name     :: String , params  :: [String] }
          | Module { id :: ID, name     :: String                       }
          | Lambda { id :: ID, inputs   :: [Type] , outputs :: [Type]   }
          | Cons   { id :: ID, segments :: [String]                     }
          | App    { id :: ID, src      :: Type   , args      :: [Type] }
          -- | List
          -- | Map
          deriving (Show, Eq, Generic)


instance QShow Type



type Traversal m = (Functor m, Applicative m, Monad m)

traverseM :: Traversal m => (Type -> m Type) -> Type -> m Type
traverseM ftype t = case t of
    Var        {}                            -> pure t
    Tuple      id items                      -> Tuple id <$> mapM ftype items
    Class      {}                            -> pure t
    Module     {}                            -> pure t
    Lambda     id inputs outputs             -> Lambda id <$> mapM ftype inputs <*> mapM ftype outputs
    Cons       {}                            -> pure t
    App        id src args                   -> App id <$> ftype src <*> mapM ftype args
    _                                        -> fail "Unexpected type"

traverseM_ :: Traversal m => (Type -> m b) -> Type -> m ()
traverseM_ ftype t = case t of
    Var        {}                            -> pure ()
    Tuple      id items                      -> pure () <* mapM_ ftype items
    Class      {}                            -> pure ()
    Module     {}                            -> pure ()
    Lambda     id inputs outputs             -> pure () <* mapM_ ftype inputs <* mapM_ ftype outputs
    Cons       {}                            -> pure ()
    App        id src args                   -> pure () <* ftype src <* mapM_ ftype args
    _                                        -> fail "Unexpected type"


