---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, DeriveGeneric, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Data.AST.Expr where

import           Flowbox.Prelude                 hiding (id, drop)
import           Flowbox.Luna.Data.AST.Type        (Type)
import qualified Flowbox.Luna.Data.AST.Lit       as Lit
import qualified Flowbox.Luna.Data.AST.Pat       as Pat
import           Flowbox.Luna.Data.AST.Utils       (ID)
import           Flowbox.Generics.Deriving.QShow   
import           GHC.Generics                      (Generic)
import           Control.Applicative               
     

type Lit         = Lit.Lit
type Pat         = Pat.Pat
type Traversal m = (Functor m, Applicative m, Monad m)


data Expr  = NOP        { id :: ID                                                                                         }
           | Accessor   { id :: ID, src       :: Expr     , dst       :: Expr                                              }
           | App        { id :: ID, src       :: Expr     , args      :: [Expr]                                            }
           | AppCons_   { id :: ID, args      :: [Expr]                                                                    }
           | Assignment { id :: ID, pat       :: Pat      , dst       :: Expr                                              }
           | Class      { id :: ID, cls       :: Type     , classes   :: [Expr] , fields    :: [Expr] , methods :: [Expr]  }
           | Cons       { id :: ID, name      :: String                                                                    }
           | Field      { id :: ID, name      :: String   , cls       :: Type                                              }
           | Function   { id :: ID, name      :: String   , pats      :: [Pat]  , output    :: Type   ,  body    :: [Expr] }
           | Import     { id :: ID, path      :: Type     , target    :: Expr   , rename    :: Maybe String                }
           | Infix      { id :: ID, name      :: String   , src       :: Expr   , dst       :: Expr                        }                                                               
           | Lambda     { id :: ID, pats      :: [Pat]    , output    :: Type   , body      :: [Expr]                      }
           | List       { id :: ID, items     :: [Expr]                                                                    }
           | Lit        { id :: ID, value     :: Lit                                                                       }
           | Tuple      { id :: ID, items     :: [Expr]                                                                    }
           | Typed      { id :: ID, cls       :: Type     , expr      :: Expr                                              }
           | Var        { id :: ID, name      :: String                                                                    }
           | Wildcard   { id :: ID                                                                                         }
           deriving (Show, Eq, Generic)

instance QShow Expr


callConstructor :: ID -> Expr -> Expr -> Expr
callConstructor id' src' arg' = case src' of
    (AppCons_ id'' args') -> AppCons_ id'' (args' ++ [arg'])
    _                     -> AppCons_ id' (src':[arg'])


aftermatch :: Expr -> Expr
aftermatch x = case x of
    AppCons_ id' (a:as) -> App id' a as
    _                   -> x


addMethod :: Expr -> Expr -> Expr
addMethod method e = e { methods = method : methods e }


addField :: Expr -> Expr -> Expr
addField field e = e { fields = field : fields e }


addClass :: Expr -> Expr -> Expr
addClass ncls e = e { classes = ncls : classes e }


traverseM :: Traversal m => (Expr -> m Expr) -> (Type -> m Type) -> (Pat -> m Pat) -> (Lit -> m Lit) -> Expr -> m Expr
traverseM fexp ftype fpat flit e = case e of
    Accessor   id' src' dst'                      -> Accessor   id'       <$> fexp src'  <*> fexp dst'
    App        id' src' args'                     -> App        id'       <$> fexp src'  <*> fexpMap args'
    Assignment id' pat' dst'                      -> Assignment id'       <$> fpat pat'  <*> fexp dst'
    Class      id' cls' classes' fields' methods' -> Class      id'       <$> ftype cls' <*> fexpMap classes' <*> fexpMap fields' <*> fexpMap methods'
    Cons       {}                                 -> pure e
    Field      id' name' cls'                     -> Field      id' name' <$> ftype cls'
    Function   id' name' pats' output' body'      -> Function   id' name' <$> fpatMap pats' <*> ftype output' <*> fexpMap body'
    Lambda     id'       pats' output' body'      -> Lambda     id'       <$> fpatMap pats' <*> ftype output' <*> fexpMap body'
    Import     {}                                 -> pure e
    Infix      id' name' src' dst'                -> Infix      id' name' <$> fexp src'     <*> fexp dst'
    List       id' items'                         -> List       id'       <$> fexpMap items'
    Lit        id' val'                           -> Lit        id'       <$> flit val'
    Tuple      id' items'                         -> Tuple      id'       <$> fexpMap items'
    Typed      id' cls' expr'                     -> Typed      id'       <$> ftype cls' <*> fexp expr'
    Var        {}                                 -> pure e
    Wildcard   {}                                 -> pure e
    NOP        {}                                 -> pure e
    AppCons_   {}                                 -> pure e
    where fexpMap = mapM fexp
          fpatMap = mapM fpat


traverseM_ :: Traversal m => (Expr -> m a) -> (Type -> m b) -> (Pat -> m c) -> (Lit -> m d) -> Expr -> m ()
traverseM_ fexp ftype fpat flit e = case e of
    Accessor   _  src' dst'                       -> drop <* fexp src'  <* fexp dst'
    App        _  src' args'                      -> drop <* fexp src'  <* fexpMap args'
    Assignment _  pat' dst'                       -> drop <* fpat pat'  <* fexp dst'
    Class      _  cls' classes' fields' methods'  -> drop <* ftype cls' <* fexpMap classes' <* fexpMap fields' <* fexpMap methods'
    Cons       {}                                 -> drop
    Field      _  _ cls'                          -> drop <* ftype cls'
    Function   _  _ pats' output' body'           -> drop <* fpatMap pats' <* ftype output' <* fexpMap body'
    Lambda     _        pats' output' body'       -> drop <* fpatMap pats' <* ftype output' <* fexpMap body'
    Import     {}                                 -> drop
    Infix      _  _ src' dst'                     -> drop <* fexp src'     <* fexp dst'
    List       _  items'                          -> drop <* fexpMap items'
    Lit        _  val'                            -> drop <* flit val'
    Tuple      _  items'                          -> drop <* fexpMap items'
    Typed      _  cls' expr'                      -> drop <* ftype cls' <* fexp expr'
    Var        {}                                 -> drop
    Wildcard   {}                                 -> drop
    NOP        {}                                 -> drop
    AppCons_   {}                                 -> drop
    where drop    = pure ()
          fexpMap = mapM_ fexp
          fpatMap = fpatMap


traverseM' :: Traversal m => (Expr -> m Expr) -> Expr -> m Expr
traverseM' fexp e = traverseM fexp pure pure pure e


traverseM'_ :: Traversal m => (Expr -> m ()) -> Expr -> m ()
traverseM'_ fexp e = traverseM_ fexp pure pure pure e