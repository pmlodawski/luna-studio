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
           | Con        { id :: ID, name      :: String                                                                    }
           | Function   { id :: ID, path      :: [String] , name      :: String   , inputs    :: [Expr] , output    :: Type   ,  body    :: [Expr] }
           | Import     { id :: ID, path      :: [String] , target    :: Expr   , rename    :: Maybe String                }
           | Infix      { id :: ID, name      :: String   , src       :: Expr   , dst       :: Expr                        }                                                               
           | Lambda     { id :: ID, pats      :: [Pat]    , output    :: Type   , body      :: [Expr]                      }
           | List       { id :: ID, items     :: [Expr]                                                                    }
           | Lit        { id :: ID, lvalue    :: Lit                                                                       }
           | Tuple      { id :: ID, items     :: [Expr]                                                                    }
           | Typed      { id :: ID, cls       :: Type     , expr      :: Expr                                              }
           | Var        { id :: ID, name      :: String                                                                    }
           | Wildcard   { id :: ID                                                                                         }
           
           | Field      { id :: ID, name      :: String   , cls       :: Type   , value     :: Maybe Expr                  }
           | Arg        { id :: ID, pat       :: Pat      , value     :: Maybe Expr                                        }
           | Native     { id :: ID, segments  :: [Expr]                                                                   }
           | NativeCode { id :: ID, code      :: String }
           | NativeVar  { id :: ID, name      :: String }

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
    Con        {}                                 -> pure e
    Field      id' name' cls' value'              -> Field      id' name' <$> ftype cls' <*> fexpMap value' 
    Function   id' path' name' inputs' output' 
               body'                              -> Function   id' path' name' <$> fexpMap inputs' <*> ftype output' <*> fexpMap body'
    Lambda     id'       pats' output' body'      -> Lambda     id'       <$> fpatMap pats' <*> ftype output' <*> fexpMap body'
    Import     {}                                 -> pure e
    Infix      id' name' src' dst'                -> Infix      id' name' <$> fexp src'     <*> fexp dst'
    List       id' items'                         -> List       id'       <$> fexpMap items'
    Lit        id' val'                           -> Lit        id'       <$> flit val'
    Tuple      id' items'                         -> Tuple      id'       <$> fexpMap items'
    Typed      id' cls' expr'                     -> Typed      id'       <$> ftype cls' <*> fexp expr'
    Native     id' segments'                      -> Native     id'       <$> fexpMap segments'
    NativeCode {}                                 -> pure e
    NativeVar  {}                                 -> pure e
    Var        {}                                 -> pure e
    Wildcard   {}                                 -> pure e
    NOP        {}                                 -> pure e
    AppCons_   {}                                 -> pure e
    Arg        id' pat' value'                    -> Arg        id'       <$> fpat pat' <*> fexpMap value' 
    where fexpMap = mapM fexp
          fpatMap = mapM fpat


traverseM_ :: Traversal m => (Expr -> m a) -> (Type -> m b) -> (Pat -> m c) -> (Lit -> m d) -> Expr -> m ()
traverseM_ fexp ftype fpat flit e = case e of
    Accessor   _  src' dst'                       -> drop <* fexp src'  <* fexp dst'
    App        _  src' args'                      -> drop <* fexp src'  <* fexpMap args'
    Assignment _  pat' dst'                       -> drop <* fpat pat'  <* fexp dst'
    Class      _  cls' classes' fields' methods'  -> drop <* ftype cls' <* fexpMap classes' <* fexpMap fields' <* fexpMap methods'
    Con        {}                                 -> drop
    Field      _ _ cls' value'                    -> drop <* ftype cls' <* fexpMap value' 
    Function   _ _ _ inputs' output' body'        -> drop <* fexpMap inputs' <* ftype output' <* fexpMap body'
    Lambda     _        pats' output' body'       -> drop <* fpatMap pats' <* ftype output' <* fexpMap body'
    Import     {}                                 -> drop
    Infix      _  _ src' dst'                     -> drop <* fexp src'     <* fexp dst'
    List       _  items'                          -> drop <* fexpMap items'
    Lit        _  val'                            -> drop <* flit val'
    Tuple      _  items'                          -> drop <* fexpMap items'
    Typed      _  cls' expr'                      -> drop <* ftype cls' <* fexp expr'
    Native     _ segments'                        -> drop <* fexpMap segments'
    NativeCode {}                                 -> drop
    NativeVar  {}                                 -> drop
    Var        {}                                 -> drop
    Wildcard   {}                                 -> drop
    NOP        {}                                 -> drop
    AppCons_   {}                                 -> drop
    Arg        _ pat' value'                      -> drop <* fpat pat' <* fexpMap value' 
    where drop    = pure ()
          fexpMap = mapM_ fexp
          fpatMap = fpatMap


traverseM' :: Traversal m => (Expr -> m Expr) -> Expr -> m Expr
traverseM' fexp e = traverseM fexp pure pure pure e


traverseM'_ :: Traversal m => (Expr -> m ()) -> Expr -> m ()
traverseM'_ fexp e = traverseM_ fexp pure pure pure e