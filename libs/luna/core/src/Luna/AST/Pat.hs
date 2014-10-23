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

module Luna.AST.Pat where

import           Control.Applicative
import qualified Data.List           as List
import           GHC.Generics

import           Flowbox.Generics.Deriving.QShow
import           Flowbox.Prelude                 hiding (Traversal, drop, id)
import           Luna.AST.Common                 (ID)
import qualified Luna.AST.Lit                    as Lit
import           Luna.AST.Prop                   (HasName)
import qualified Luna.AST.Prop                   as Prop
import           Luna.AST.Type                   (Type)
import qualified Luna.AST.Type                   as Type


type Lit = Lit.Lit

data Pat = Var             { _id :: ID, _name  :: String                          }
         | Lit             { _id :: ID, _value :: Lit                             }
         | Tuple           { _id :: ID, _items :: [Pat]                           }
         | Con             { _id :: ID, _name  :: String                          }
         | App             { _id :: ID, _src   :: Pat       , _args      :: [Pat] }
         | Typed           { _id :: ID, _pat   :: Pat       , _cls       :: Type  }
         | Wildcard        { _id :: ID                                            }
         | Grouped         { _id :: ID, _pat   :: Pat                             }
         | RecWildcard     { _id :: ID                                            }
         deriving (Show, Eq, Generic, Read)

instance QShow Pat
makeLenses (''Pat)


var :: String -> ID -> Pat
var = flip Var


type Traversal m = (Functor m, Applicative m, Monad m)

traverseM :: Traversal m => (Pat -> m Pat) -> (Type -> m Type) -> (Lit -> m Lit) -> Pat -> m Pat
traverseM fpat ftype flit p = case p of
    Lit         id' val'                      -> Lit     id' <$> flit val'
    Tuple       id' items'                    -> Tuple   id' <$> fpatMap items'
    App         id' src' args'                -> App     id' <$> fpat src' <*> fpatMap args'
    Typed       id' pat' cls'                 -> Typed   id' <$> fpat pat' <*> ftype cls'
    Grouped     id' pat'                      -> Grouped id' <$> fpat pat'
    Var         {}                            -> pure p
    Con         {}                            -> pure p
    Wildcard    {}                            -> pure p
    RecWildcard {}                            -> pure p
    where fpatMap = mapM fpat

traverseM_ :: Traversal m => (Pat -> m c) -> (Type -> m b) -> (Lit -> m d) -> Pat -> m ()
traverseM_ fpat ftype flit p = case p of
    Lit         _  val'                       -> drop <* flit val'
    Tuple       _  items'                     -> drop <* fpatMap items'
    App         _  src' args'                 -> drop <* fpat src' <* fpatMap args'
    Typed       _  pat' cls'                  -> drop <* fpat pat' <* ftype cls'
    Grouped     _  pat'                       -> drop <* fpat pat'
    Var         {}                            -> drop
    Con         {}                            -> drop
    Wildcard    {}                            -> drop
    RecWildcard {}                            -> drop
    where drop    = pure ()
          fpatMap = mapM_ fpat


traverseM' :: Traversal m => (Pat -> m Pat) -> Pat -> m Pat
traverseM' fpat = traverseM fpat pure pure


traverseM'_ :: Traversal m => (Pat -> m ()) -> Pat -> m ()
traverseM'_ fpat = traverseM_ fpat pure pure


traverseMR :: Traversal m => (Pat -> m Pat) -> (Type -> m Type) -> (Lit -> m Lit) -> Pat -> m Pat
traverseMR fpat ftype flit = tfpat where
    tfpat p = fpat =<< traverseM tfpat tftype flit p
    tftype  = Type.traverseMR ftype


-- FIXME[PM]: make typeclass, gather all similiar lunaShows and move somewhere else
lunaShow :: Pat -> String
lunaShow p = case p of
    Var      _ name'      -> name'
    Lit      _ value'     -> Lit.lunaShow value'
    Tuple    _ items'     -> List.intercalate ", " strs where
                                   strs = map lunaShow items'
    Con      _ name'      -> name'
    App      _ src' args' -> srcStr ++ " " ++ unwords argStrs where
                                   argStrs = map lunaShow args'
                                   srcStr  = lunaShow src'
    Typed    _ pat' cls'  -> patStr ++ " :: " ++ typeStr where
                                   patStr = lunaShow pat'
                                   typeStr = Type.lunaShow cls'
    Grouped  _ pat'       -> concat ["(", lunaShow pat', ")"]
    Wildcard _            -> "_"
    RecWildcard _         -> ".."


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance HasName Pat where
    name = _name
