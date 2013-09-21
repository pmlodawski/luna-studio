---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, ConstraintKinds #-}

module Flowbox.Luna.Data.AST.Pat where

import           Flowbox.Prelude                 hiding (id, drop)
import qualified Flowbox.Luna.Data.AST.Lit       as Lit
import           Flowbox.Luna.Data.AST.Type        (Type)
import           Flowbox.Luna.Data.AST.Utils       (ID)
import           Flowbox.Generics.Deriving.QShow   
import           GHC.Generics                      
import           Control.Applicative               

type Lit = Lit.Lit

data Pat = Var             { id :: ID, name      :: String                         }
         | Lit             { id :: ID, value     :: Lit                            }
         | Tuple           { id :: ID, items     :: [Pat]                          }
         | Cons            { id :: ID, name      :: String                         }
         | App             { id :: ID, src       :: Pat       , args      :: [Pat] }
         | Typed           { id :: ID, pat       :: Pat       , cls       :: Type  }
         | Wildcard        { id :: ID                                              }
         deriving (Show, Eq, Generic)

instance QShow Pat


type Traversal m = (Functor m, Applicative m, Monad m)

traverseM :: Traversal m => (Pat -> m Pat) -> (Type -> m Type) -> (Lit -> m Lit) -> Pat -> m Pat
traverseM fpat ftype flit p = case p of
    Lit        id' val'                      -> Lit   id' <$> flit val'
    Tuple      id' items'                    -> Tuple id' <$> fpatMap items'
    App        id' src' args'                -> App   id' <$> fpat src' <*> fpatMap args'
    Typed      id' pat' cls'                 -> Typed id' <$> fpat pat' <*> ftype cls'
    Var        {}                            -> pure p
    Cons       {}                            -> pure p
    Wildcard   {}                            -> pure p
    where fpatMap = mapM fpat

traverseM_ :: Traversal m => (Pat -> m c) -> (Type -> m b) -> (Lit -> m d) -> Pat -> m ()
traverseM_ fpat ftype flit p = case p of
    Lit        _  val'                       -> drop <* flit val'
    Tuple      _  items'                     -> drop <* fpatMap items'
    App        _  src' args'                 -> drop <* fpat src' <* fpatMap args'
    Typed      _  pat' cls'                  -> drop <* fpat pat' <* ftype cls'
    Var        {}                            -> drop
    Cons       {}                            -> drop
    Wildcard   {}                            -> drop
    where drop    = pure ()
          fpatMap = mapM_ fpat


traverseM' :: Traversal m => (Pat -> m Pat) -> Pat -> m Pat
traverseM' fpat p = traverseM fpat pure pure p


traverseM'_ :: Traversal m => (Pat -> m ()) -> Pat -> m ()
traverseM'_ fpat p = traverseM_ fpat pure pure p


