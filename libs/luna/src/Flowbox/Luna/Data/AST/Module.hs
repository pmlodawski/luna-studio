---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, ConstraintKinds #-}

module Flowbox.Luna.Data.AST.Module where

import           Flowbox.Prelude                 hiding (id, drop, mod)
import qualified Flowbox.Luna.Data.AST.Expr      as Expr
import           Flowbox.Luna.Data.AST.Expr        (Expr)
import qualified Flowbox.Luna.Data.AST.Type      as Type
import qualified Flowbox.Luna.Data.AST.Lit       as Lit
import qualified Flowbox.Luna.Data.AST.Pat       as Pat
import           Flowbox.Luna.Data.AST.Utils       (ID)
import           GHC.Generics                      (Generic)
import           Flowbox.Generics.Deriving.QShow   
import           Control.Applicative               

data Module = Module { id      :: ID
                     , cls     :: Type     
                     , imports :: [Expr] 
                     , classes :: [Expr] 
                     , fields  :: [Expr] 
                     , methods :: [Expr] 
                     , modules :: [Module] 
                     } deriving (Show, Generic)

instance QShow Module

type Lit         = Lit.Lit
type Pat         = Pat.Pat
type Type        = Type.Type
type Traversal m = (Functor m, Applicative m, Monad m)


mk :: ID -> Type -> Module
mk id' mod = Module id' mod [] [] [] [] []


mkClass :: Module -> Expr
mkClass (Module id' (Type.Module tid path) _ classes' fields' methods' _) = 
    Expr.Class id' (Type.Class tid (last path) []) classes' fields' methods'


addMethod :: Expr -> Module -> Module
addMethod method mod = mod { methods = method : methods mod }


addField :: Expr -> Module -> Module
addField field mod = mod { fields = field : fields mod }

addClass :: Expr -> Module -> Module
addClass ncls mod = mod { classes = ncls : classes mod }


addImport :: Expr -> Module -> Module
addImport imp mod = mod { imports = imp : imports mod }


traverseM :: Traversal m => (Module -> m Module) -> (Expr -> m Expr) -> (Type -> m Type) -> (Pat -> m Pat) -> (Lit -> m Lit) -> Module -> m Module
traverseM fmod fexp ftype _{-fpat-} _{-flit-} mod = case mod of
    Module     id' cls' imports' classes'             
               fields' methods' modules'     ->  Module id' 
                                                 <$> ftype cls' 
                                                 <*> fexpMap imports' 
                                                 <*> fexpMap classes' 
                                                 <*> fexpMap fields' 
                                                 <*> fexpMap methods' 
                                                 <*> fmodMap modules'
    where fexpMap = mapM fexp
          fmodMap = mapM fmod

traverseM_ :: Traversal m => (Module -> m a) -> (Expr -> m b) -> (Type -> m c) -> (Pat -> m d) -> (Lit -> m e) -> Module -> m ()
traverseM_ fmod fexp ftype _{-fpat-} _{-flit-} mod = case mod of
    Module     _ cls' imports' classes'             
               fields' methods' modules'     -> drop 
                                                <* ftype cls' 
                                                <* fexpMap imports'
                                                <* fexpMap classes' 
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