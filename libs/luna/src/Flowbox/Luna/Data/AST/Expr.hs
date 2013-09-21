---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, DeriveGeneric, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Data.AST.Expr where

import           Flowbox.Prelude                 hiding (id)
import           Flowbox.Luna.Data.AST.Type        (Type)
import qualified Flowbox.Luna.Data.AST.Lit       as Lit
import qualified Flowbox.Luna.Data.AST.Pat       as Pat
import           Flowbox.Luna.Data.AST.Utils       (ID)
import           Data.Typeable                     
import           Flowbox.Generics.Deriving.QShow   
import           GHC.Generics                      (Generic)
import           Control.Applicative               

import           Text.Parsec                     hiding (parse, many, optional, (<|>))

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
addMethod method expr = expr { methods = method : methods expr }


addField :: Expr -> Expr -> Expr
addField field expr = expr { fields = field : fields expr }


addClass :: Expr -> Expr -> Expr
addClass ncls expr = expr { classes = ncls : classes expr }


traverseM :: Traversal m => (Expr -> m Expr) -> (Type -> m Type) -> (Pat -> m Pat) -> (Lit -> m Lit) -> Expr -> m Expr
traverseM fexp ftype fpat flit expr = case expr of
    Accessor   id src dst                    -> Accessor   id      <$> fexp src  <*> fexp dst
    App        id src args                   -> App        id      <$> fexp src  <*> fexpMap args
    Assignment id pat dst                    -> Assignment id      <$> fpat pat  <*> fexp dst
    Class      id cls classes fields methods -> Class      id      <$> ftype cls <*> fexpMap classes <*> fexpMap fields <*> fexpMap methods
    Cons       {}                            -> pure expr
    Field      id name cls                   -> Field      id name <$> ftype cls
    Function   id name pats output body      -> Function   id name <$> fpatMap pats <*> ftype output <*> fexpMap body
    Lambda     id      pats output body      -> Lambda     id      <$> fpatMap pats <*> ftype output <*> fexpMap body
    Import     {}                            -> pure expr
    Infix      id name src dst               -> Infix      id name <$> fexp src     <*> fexp dst
    List       id items                      -> List       id      <$> fexpMap items
    Lit        id val                        -> Lit        id      <$> flit val
    --Module     id cls imports classes             
    --           fields methods modules        -> Module     id      <$> ftype cls <*> fexpMap imports <*> fexpMap classes <*> fexpMap fields <*> fexpMap methods <*> fexpMap modules
    Tuple      id items                      -> Tuple      id      <$> fexpMap items
    Typed      id cls expr                   -> Typed      id      <$> ftype cls <*> fexp expr
    Var        {}                            -> pure expr
    NOP        {}                            -> pure expr
    _                                        -> fail "Unexpected expression"
    where fexpMap = mapM fexp
          fpatMap = mapM fpat

traverseM_ :: Traversal m => (Expr -> m a) -> (Type -> m b) -> (Pat -> m c) -> (Lit -> m d) -> Expr -> m ()
traverseM_ fexp ftype fpat flit expr = case expr of
    Accessor   id src dst                    -> drop <* fexp src  <* fexp dst
    App        id src args                   -> drop <* fexp src  <* fexpMap args
    Assignment id pat dst                    -> drop <* fpat pat  <* fexp dst
    Class      id cls classes fields methods -> drop <* ftype cls <* fexpMap classes <* fexpMap fields <* fexpMap methods
    Cons       {}                            -> drop
    Field      id name cls                   -> drop <* ftype cls
    Function   id name pargs output body     -> drop <* fpatMap pargs <* ftype output <* fexpMap body
    Lambda     id      pargs output body     -> drop <* fpatMap pargs <* ftype output <* fexpMap body
    Import     {}                            -> drop
    Infix      id name src dst               -> drop <* fexp src       <* fexp dst
    List       id items                      -> drop <* fexpMap items
    Lit        id val                        -> drop <* flit val
    --Module     id cls imports classes             
    --           fields methods modules        -> drop <* ftype cls <* fexpMap imports <* fexpMap classes <* fexpMap fields <* fexpMap methods <* fexpMap modules
    Tuple      id items                      -> drop <* fexpMap items
    Typed      id cls expr                   -> drop <* ftype cls <* fexp expr
    Var        {}                            -> drop
    NOP        {}                            -> drop
    _                                        -> fail "Unexpected expression"
    where drop    = pure ()
          fexpMap = mapM_ fexp
          fpatMap = fpatMap


traverseM' :: Traversal m => (Expr -> m Expr) -> Expr -> m Expr
traverseM' fexp expr = traverseM fexp pure pure pure expr



traverseM'_ :: Traversal m => (Expr -> m ()) -> Expr -> m ()
traverseM'_ fexp expr = traverseM_ fexp pure pure pure expr


--traverse :: (Expr -> Expr) -> Expr -> Expr
--traverse fexp expr = let t = traverse fexp in case expr of
--    Assignment id pat dst                    -> Assignment id pat (t dst)
--    Tuple      id items                      -> Tuple      id (map t items)
--    Typed      id cls expr                   -> Typed      id cls (t expr)
--    App        id src args                   -> App        id (t src) (map t args)
--    Accessor   id src dst                    -> Accessor   id (t src) (t dst)
--    Infix      id name src dst               -> Infix      id name (t src) (t dst)
--    Class      id cls classes fields methods -> Class      id cls (map t classes) (map t fields) (map t methods)
--    Module     id cls imports classes             
--               fields methods modules        -> Module     id cls (map t imports) (map t classes) (map t fields) (map t methods) (map t modules)
--    Lambda     id signature body             -> Lambda     id signature (map t body)
--    Function   id name signature body        -> Function   id name signature (map t body)
--    List       id items                      -> List       id (map t items)
--    _                                        -> expr

--transform :: (Expr -> Expr) -> Expr -> Expr
--transform f expr = let t = transform f in case expr of
--    (Assignment id pat dst)                    -> f (Assignment id pat (t dst))
--    (Tuple      id items)                      -> f (Tuple      id (map t items))
--    (Typed      id cls expr)                   -> f (Typed      id cls (t expr))
--    (App        id src args)                   -> f (App        id (t src) (map t args))
--    (Accessor   id src dst)                    -> f (Accessor   id (t src) (t dst))
--    (Infix      id name src dst)               -> f (Infix      id name (t src) (t dst))
--    (Class      id cls classes fields methods) -> f (Class      id cls (map t classes) (map t fields) (map t methods))
--    (Module     id cls imports classes        
--                fields methods modules)        -> f (Module     id cls (map t imports) (map t classes) (map t fields) (map t methods) (map t modules))
--    (Lambda     id signature body)             -> f (Lambda     id signature (map t body))
--    (Function   id name signature body)        -> f (Function   id name signature (map t body))
--    (List       id items)                      -> f (List       id (map t items))
--    _                                          -> f expr

--transformM :: (Functor m, Applicative m, Monad m) => (Expr -> m Expr) -> Expr -> m Expr
--transformM f expr = let t = transformM f in case expr of
--    (Module     id cls imports classes                   
--                fields methods modules)        -> f =<< (Module     id cls <$> mapM t imports <*> mapM t classes <*> mapM t fields <*> mapM t methods <*> mapM t modules)
--    (Assignment id pat dst)                    -> f =<< (Assignment id pat <$> t dst)
--    (Tuple      id items)                      -> f =<< (Tuple      id <$> mapM t items)
--    (Typed      id cls expr)                   -> f =<< (Typed      id cls <$> t expr)
--    (App        id src args)                   -> f =<< (App        id <$> t src <*> mapM t args)
--    (Accessor   id src dst)                    -> f =<< (Accessor   id <$> t src <*> t dst)
--    (Infix      id name src dst)               -> f =<< (Infix      id name <$> t src <*> t dst)
--    (Class      id cls classes fields methods) -> f =<< (Class      id cls <$> mapM t classes <*> mapM t fields <*> mapM t methods)
--    (Lambda     id signature body)             -> f =<< (Lambda     id signature <$> mapM t body)
--    (Function   id name signature body)        -> f =<< (Function   id name signature <$> mapM t body)
--    (List       id items)                      -> f =<< (List       id <$> mapM t items)
--    _                                          -> f (expr)


--transformM2 :: (Functor m, Applicative m, Monad m) => (Expr -> m Expr) -> Expr -> m Expr
--transformM2 f expr = do
--    let t = transformM f
--    e <- f expr
--    case e of
--      (Module     id cls imports classes                   
--                  fields methods modules)        -> (Module     id cls <$> mapM t imports <*> mapM t classes <*> mapM t fields <*> mapM t methods <*> mapM t modules)
--      (Assignment id pat dst)                    -> (Assignment id pat <$> t dst)
--      (Tuple      id items)                      -> (Tuple      id <$> mapM t items)
--      (Typed      id cls expr)                   -> (Typed      id cls <$> t expr)
--      (App        id src args)                   -> (App        id <$> t src <*> mapM t args)
--      (Accessor   id src dst)                    -> (Accessor   id <$> t src <*> t dst)
--      (Infix      id name src dst)               -> (Infix      id name <$> t src <*> t dst)
--      (Class      id cls classes fields methods) -> (Class      id cls <$> mapM t classes <*> mapM t fields <*> mapM t methods)
--      (Lambda     id signature body)             -> (Lambda     id signature <$> mapM t body)
--      (Function   id name signature body)        -> (Function   id name signature <$> mapM t body)
--      (List       id items)                      -> (List       id <$> mapM t items)
--      _                                          -> (pure expr)




