---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, ConstraintKinds #-}

module Flowbox.Luna.Data.AST.Type where

import qualified Data.List as List
import           GHC.Generics                      

import           Flowbox.Prelude                 hiding (id, drop, Traversal)
import           Flowbox.Generics.Deriving.QShow   
import           Flowbox.Luna.Data.AST.Utils       (ID)

data Type = Unknown { _id :: ID                                               }
          | Var     { _id :: ID, _name     :: String                          }
          | Tuple   { _id :: ID, _items    :: [Type]                          }
          | Data    { _id :: ID, _name     :: String   , _params  :: [String] }
          | Module  { _id :: ID, _path     :: [String]                        }
          | Lambda  { _id :: ID, _inputs   :: [Type]   , _output  :: Type     }
          | Con     { _id :: ID, _segments :: [String]                        }
          | App     { _id :: ID, _src      :: Type     , _args    :: [Type]   }
          deriving (Show, Eq, Generic)


instance QShow Type
makeLenses (''Type)

type Traversal m = (Functor m, Applicative m, Monad m)


getNameID :: Type -> String
getNameID t = case t of
    Module {} -> last (view path t)
    Data   {} -> view name t
    _         -> "undefinedName"


traverseM :: Traversal m => (Type -> m Type) -> Type -> m Type
traverseM ftype t = case t of
    Tuple      id' items'                    -> Tuple  id' <$> ftypeMap items'
    Lambda     id' inputs' output'           -> Lambda id' <$> ftypeMap inputs' <*> ftype output'
    App        id' src' args'                -> App    id' <$> ftype    src'    <*> ftypeMap args'
    Var        {}                            -> pure t
    Data       {}                            -> pure t
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
    Data       {}                            -> drop
    Module     {}                            -> drop
    Con        {}                            -> drop
    Unknown    {}                            -> drop
    where drop     = pure ()
          ftypeMap = mapM_ ftype


lunaShow :: Type -> String
lunaShow t = case t of
    Unknown _               -> "Unknown"
    Var     _ name'         -> name'
    Tuple   _ items'        -> "{" ++ (List.intercalate ", " strs) ++ "}" where
                                   strs = map lunaShow items'
    --Class   _ name' params' -> name' ++ " " ++ (List.intercalate " " params')
    --Module  _ path'         -> List.intercalate "." path'
    Con     _ segments'     -> List.intercalate "." segments'
