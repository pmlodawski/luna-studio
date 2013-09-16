---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, DeriveGeneric, NoMonomorphismRestriction #-}

module Flowbox.Luna.AST.Expr where

import           Flowbox.Prelude                 hiding (id)
import           Flowbox.Luna.AST.Type             (Type)
import qualified Flowbox.Luna.AST.Lit            as Lit
import qualified Flowbox.Luna.AST.Pat            as Pat
import           Flowbox.Luna.AST.Utils            (ID)
import           Data.Typeable                     
import           Flowbox.Generics.Deriving.QShow   
import           GHC.Generics                      (Generic)
import           Control.Applicative               

import           Text.Parsec                     hiding (parse, many, optional, (<|>))

type Lit = Lit.Lit
type Pat = Pat.Pat

data Expr  = NOP        { id :: ID                                                                                                                                }
           | Import     { id :: ID, segments  :: [String] , name      :: String                                                                                   }
           | Var        { id :: ID, name      :: String                                                                                                           }
           | Lit        { id :: ID, value     :: Lit                                                                                                              }
           | Assignment { id :: ID, pat       :: Pat      , dst       :: Expr                                                                                     }
           | Tuple      { id :: ID, items     :: [Expr]                                                                                                           }
           | Typed      { id :: ID, cls       :: Type     , expr      :: Expr                                                                                     }
           | App        { id :: ID, src       :: Expr     , args      :: [Expr]                                                                                   }
           | AppCons_   { id :: ID, args      :: [Expr]                                                                                                           }
           | Accessor   { id :: ID, src       :: Expr     , dst       :: Expr                                                                                     }
           | Infix      { id :: ID, name      :: String   , src       :: Expr   , dst     :: Expr                                                                 }                                                               
           | Class      { id :: ID, cls       :: Type     , classes   :: [Expr] , fields    :: [Expr] , methods :: [Expr]                                         }
           | Module     { id :: ID, cls       :: Type     , imports   :: [Expr] , classes   :: [Expr] , fields  :: [Expr] , methods :: [Expr] , modules :: [Expr] }
           | Field      { id :: ID, name      :: String   , cls       :: Type                                                                                     }
           | Lambda     { id :: ID, signature :: [Pat]    , body      :: [Expr]                                                                                   }
           | Cons       { id :: ID, segments  :: [String]                                                                                                         }
           | Function   { id :: ID, name      :: String   , signature :: [Pat]   , body    :: [Expr]                                                              }
           | List       { id :: ID, items     :: [Expr]                                                                                                           }
           deriving (Show, Eq, Generic)


instance QShow Expr

traverse :: (Expr -> Expr) -> Expr -> Expr
traverse fexp expr = let t = traverse fexp in case expr of
    Assignment id pat dst                    -> Assignment id pat (t dst)
    Tuple      id items                      -> Tuple      id (map t items)
    Typed      id cls expr                   -> Typed      id cls (t expr)
    App        id src args                   -> App        id (t src) (map t args)
    Accessor   id src dst                    -> Accessor   id (t src) (t dst)
    Infix      id name src dst               -> Infix      id name (t src) (t dst)
    Class      id cls classes fields methods -> Class      id cls (map t classes) (map t fields) (map t methods)
    Module     id cls imports classes             
               fields methods modules        -> Module     id cls (map t imports) (map t classes) (map t fields) (map t methods) (map t modules)
    Lambda     id signature body             -> Lambda     id signature (map t body)
    Function   id name signature body        -> Function   id name signature (map t body)
    List       id items                      -> List       id (map t items)
    _                                        -> expr

traverseM :: (Functor m, Applicative m, Monad m) => (Expr -> m Expr) -> Expr -> m Expr
traverseM fexp expr = case expr of
    Assignment id pat dst                    -> Assignment id pat <$> fexp dst
    Tuple      id items                      -> Tuple      id <$> mapM fexp items
    Typed      id cls expr                   -> Typed      id cls <$> fexp expr
    App        id src args                   -> App        id <$> fexp src <*> mapM fexp args
    Accessor   id src dst                    -> Accessor   id <$> fexp src <*> fexp dst
    Infix      id name src dst               -> Infix      id name <$> fexp src <*> fexp dst
    Class      id cls classes fields methods -> Class      id cls <$> mapM fexp classes <*> mapM fexp fields <*> mapM fexp methods
    Module     id cls imports classes             
               fields methods modules        -> Module     id cls <$> mapM fexp imports <*> mapM fexp classes <*> mapM fexp fields <*> mapM fexp methods <*> mapM fexp modules
    Lambda     id signature body             -> Lambda     id signature <$> mapM fexp body
    Function   id name signature body        -> Function   id name signature <$> mapM fexp body
    List       id items                      -> List       id <$> mapM fexp items
    _                                        -> pure expr

traverseM_ :: (Functor m, Applicative m, Monad m) => (Expr -> m ()) -> Expr -> m ()
traverseM_ fexp expr = case expr of
    Assignment id pat dst                    -> fexp dst
    Tuple      id items                      -> mapM_ fexp items
    Typed      id cls expr                   -> fexp expr
    App        id src args                   -> fexp src <* mapM fexp args
    Accessor   id src dst                    -> fexp src <* fexp dst
    Infix      id name src dst               -> fexp src <* fexp dst
    Class      id cls classes fields methods -> mapM_ fexp classes <* mapM_ fexp fields <* mapM_ fexp methods
    Module     id cls imports classes             
               fields methods modules        -> mapM_ fexp imports <* mapM_ fexp classes <* mapM_ fexp fields <* mapM_ fexp methods <* mapM_ fexp modules
    Lambda     id signature body             -> mapM_ fexp body
    Function   id name signature body        -> mapM_ fexp body
    List       id items                      -> mapM_ fexp items
    _                                        -> pure ()

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


addImport :: Expr -> Expr -> Expr
addImport imp e = e { imports = imp : imports e }