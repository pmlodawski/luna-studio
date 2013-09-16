---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, ConstraintKinds #-}

module Flowbox.Luna.AST.Pat where

import           Flowbox.Prelude                   
import qualified Flowbox.Luna.AST.Lit            as Lit
import qualified Flowbox.Luna.AST.Type           as Type
import           Flowbox.Luna.AST.Type             (Type)
import           Flowbox.Luna.AST.Utils            (ID)
import           Flowbox.Generics.Deriving.QShow   
import           GHC.Generics                      
import           Control.Applicative 

type Lit = Lit.Lit

data Pat = Var             { id :: ID, name      :: String                         }
         | Lit             { id :: ID, value     :: Lit                            }
         | Tuple           { id :: ID, items     :: [Pat]                          }
         | Cons            { id :: ID, segments  :: [String]                       }
         | App             { id :: ID, src       :: Pat       , args      :: [Pat] }
         | Typed           { id :: ID, pat       :: Pat       , cls       :: Type  }
         | CallConstructor { id :: ID, args      :: [Pat]                          }
         | Wildcard        { id :: ID                                              }
         deriving (Show, Eq, Generic)

instance QShow Pat


type Traversal m = (Functor m, Applicative m, Monad m)

traverseM :: Traversal m => (Pat -> m Pat) -> (Type -> m Type) -> (Lit -> m Lit) -> Pat -> m Pat
traverseM fpat ftype flit pat = case pat of
    Var        {}                            -> pure pat
    Lit        id val                        -> Lit id <$> flit val
    Tuple      id items                      -> Tuple id <$> mapM fpat items
    Cons       {}                            -> pure pat
    App        id src args                   -> App id <$> fpat src <*> mapM fpat args
    Typed      id pat cls                    -> Typed id <$> fpat pat <*> ftype cls
    Wildcard   {}                            -> pure pat
    _                                        -> fail "Unexpected pattern"

traverseM_ :: Traversal m => (Pat -> m c) -> (Type -> m b) -> (Lit -> m d) -> Pat -> m ()
traverseM_ fpat ftype flit pat = case pat of
    Var        {}                            -> pure ()
    Lit        id val                        -> pure () <* flit val
    Tuple      id items                      -> pure () <* mapM_ fpat items
    Cons       {}                            -> pure ()
    App        id src args                   -> pure () <* fpat src <* mapM_ fpat args
    Typed      id pat cls                    -> pure () <* fpat pat <* ftype cls
    Wildcard   {}                            -> pure ()
    _                                        -> fail "Unexpected pattern"


traverseM' :: Traversal m => (Pat -> m Pat) -> Pat -> m Pat
traverseM' fpat pat = traverseM fpat pure pure pat


traverseM'_ :: Traversal m => (Pat -> m ()) -> Pat -> m ()
traverseM'_ fpat pat = traverseM_ fpat pure pure pat


