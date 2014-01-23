---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Luna.Data.AST.Module where

import           Control.Applicative
import           Flowbox.Generics.Deriving.QShow
import           Flowbox.Luna.Data.AST.Expr      (Expr)
import qualified Flowbox.Luna.Data.AST.Expr      as Expr
import           Flowbox.Luna.Data.AST.Lit       (Lit)
import           Flowbox.Luna.Data.AST.Pat       (Pat)
import           Flowbox.Luna.Data.AST.Type      (Type)
import qualified Flowbox.Luna.Data.AST.Type      as Type
import           Flowbox.Luna.Data.AST.Utils     (ID)
import           Flowbox.Prelude                 hiding (Traversal, drop, id, mod)
import           GHC.Generics                    (Generic)

type Traversal m = (Functor m, Applicative m, Monad m)

data Module = Module { _id          :: ID
                     , _cls         :: Type
                     , _imports     :: [Expr]
                     , _classes     :: [Expr]
                     , _typeAliases :: [Expr]
                     , _typeDefs    :: [Expr]
                     , _fields      :: [Expr]
                     , _methods     :: [Expr]
                     , _modules     :: [Module]
                     } deriving (Show, Generic)

instance QShow Module
makeLenses (''Module)


mk :: ID -> Type -> Module
mk id' mod = Module id' mod [] [] [] [] [] [] []

mkClass :: Module -> Expr
mkClass (Module id' (Type.Module tid path) _ classes' _ _ fields' methods' _) = 
    Expr.Data id' (Type.Data tid (last path) []) [Expr.ConD 0 (last path) fields'] classes' methods'

addMethod :: Expr -> Module -> Module
addMethod method mod = mod & methods %~ (method:)

addField :: Expr -> Module -> Module
addField field mod = mod & fields %~ (field:)

addClass :: Expr -> Module -> Module
addClass ncls mod = mod & classes %~ (ncls:)

addModule :: Module -> Module -> Module
addModule submod mod = mod & modules %~ (submod:)

addImport :: Expr -> Module -> Module
addImport imp mod = mod & imports %~ (imp:)

addTypeAlias :: Expr -> Module -> Module
addTypeAlias als mod = mod & typeAliases %~ (als:)

addTypeDef :: Expr -> Module -> Module
addTypeDef td mod = mod & typeDefs %~ (td:)


traverseM :: Traversal m => (Module -> m Module) -> (Expr -> m Expr) -> (Type -> m Type) -> (Pat -> m Pat) -> (Lit -> m Lit) -> Module -> m Module
traverseM fmod fexp ftype _{-fpat-} _{-flit-} mod = case mod of
    Module     id' cls' imports' classes' typeAliases' typeDefs'
               fields' methods' modules'     ->  Module id'
                                             <$> ftype cls'
                                             <*> fexpMap imports'
                                             <*> fexpMap classes'
                                             <*> fexpMap typeAliases'
                                             <*> fexpMap typeDefs'
                                             <*> fexpMap fields'
                                             <*> fexpMap methods'
                                             <*> fmodMap modules'
    where fexpMap = mapM fexp
          fmodMap = mapM fmod

traverseM_ :: Traversal m => (Module -> m a) -> (Expr -> m b) -> (Type -> m c) -> (Pat -> m d) -> (Lit -> m e) -> Module -> m ()
traverseM_ fmod fexp ftype _{-fpat-} _{-flit-} mod = case mod of
    Module     _ cls' imports' classes' typeAliases' typeDefs'
               fields' methods' modules'     -> drop
                                             <* ftype cls'
                                             <* fexpMap imports'
                                             <* fexpMap classes'
                                             <* fexpMap typeAliases'
                                             <* fexpMap typeDefs'
                                             <* fexpMap fields'
                                             <* fexpMap methods'
                                             <* fmodMap modules'
    where drop    = pure ()
          fexpMap = mapM_ fexp
          fmodMap = mapM_ fmod


--traverseM' :: Traversal m => (Expr -> m Expr) -> Module -> m Module
--traverseM' fexp mod = traverseM fexp pure pure pure mod


--traverseM'_ :: Traversal m => (Expr -> m ()) -> Module -> m ()
--traverseM'_ fexp mod = traverseM_ fexp pure pure pure mod
