---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Luna.AST.Type where

import qualified Data.List    as List
import           GHC.Generics

import Flowbox.Generics.Deriving.QShow
import Flowbox.Prelude                 hiding (Traversal, drop, id)
import Luna.AST.Common                 (ID)

data Type = Unknown  { _id :: ID                                               }
          | Var      { _id :: ID, _name     :: String                          }
          | Tuple    { _id :: ID, _items    :: [Type]                          }
          | List     { _id :: ID, _item     :: Type                            }
          | Data     { _id :: ID, _name     :: String   , _params  :: [String] }
          | Module   { _id :: ID, _name     :: String   , _path    :: [String] }
          | Function { _id :: ID, _inputs   :: [Type]   , _output  :: Type     }
          | Con      { _id :: ID, _segments :: [String]                        }
          | App      { _id :: ID, _src      :: Type     , _args    :: [Type]   }
          deriving (Show, Eq, Generic, Read)


instance QShow Type
makeLenses (''Type)

type Traversal m = (Functor m, Applicative m, Monad m)


traverseM :: Traversal m => (Type -> m Type) -> Type -> m Type
traverseM ftype t = case t of
    Tuple      id' items'                    -> Tuple  id' <$> ftypeMap items'
    List       id' item'                     -> List   id' <$> ftype item'
    Function   id' inputs' output'           -> Function id' <$> ftypeMap inputs' <*> ftype output'
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
    List       _  item'                      -> drop <* ftype item'
    Function   _  inputs' output'            -> drop <* ftypeMap inputs' <* ftype output'
    App        _  src' args'                 -> drop <* ftype    src'    <* ftypeMap args'
    Var        {}                            -> drop
    Data       {}                            -> drop
    Module     {}                            -> drop
    Con        {}                            -> drop
    Unknown    {}                            -> drop
    where drop     = pure ()
          ftypeMap = mapM_ ftype


traverseMR :: Traversal m => (Type -> m Type) -> Type -> m Type
traverseMR ftype t = traverseM (traverseMR ftype) t >>= ftype


lunaShow :: Type -> String
lunaShow t = case t of
    Unknown _               -> "Unknown"
    Var     _ name'         -> name'
    Tuple   _ items'        -> "(" ++ List.intercalate ", " strs ++ ")" where
                                   strs = map lunaShow items'

    List    _ item'        -> "[" ++ lunaShow item' ++ "]"
    --Class   _ name' params' -> name' ++ " " ++ (List.intercalate " " params')
    --Module  _ path'         -> List.intercalate "." path'
    Con     _ segments'     -> List.intercalate "." segments'


mkModule :: ID -> [String] -> Type
mkModule id' modulePath' = Module id' (head modulePath') (init modulePath')
