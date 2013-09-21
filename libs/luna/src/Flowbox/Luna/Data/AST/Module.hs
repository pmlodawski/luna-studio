---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, ConstraintKinds #-}

module Flowbox.Luna.Data.AST.Module where

import           Flowbox.Prelude              
import qualified Flowbox.Luna.Data.AST.Expr  as Expr
import           Flowbox.Luna.Data.AST.Expr    (Expr)
import qualified Flowbox.Luna.Data.AST.Type  as Type
import qualified Flowbox.Luna.Data.AST.Type    (Type)
import qualified Flowbox.Luna.Data.AST.Lit       as Lit
import qualified Flowbox.Luna.Data.AST.Pat       as Pat
import           Flowbox.Luna.Data.AST.Utils   (ID)
import           GHC.Generics                  (Generic)
import           Flowbox.Generics.Deriving.QShow 
import           Control.Applicative      

data Module = Module { id      :: ID
                     , cls     :: Type     
                     , imports :: [Expr] 
                     , classes :: [Expr] 
                     , fields  :: [Expr] 
                     , methods :: [Expr] 
                     , modules :: [Expr] 
                     } deriving (Show, Generic)

instance QShow Module

type Lit         = Lit.Lit
type Pat         = Pat.Pat
type Type        = Type.Type
type Traversal m = (Functor m, Applicative m, Monad m)

--mk :: Int -> String -> Module
mk id mod = Module id mod [] [] [] [] []


addMethod :: Expr -> Module -> Module
addMethod method mod = mod { methods = method : methods mod }


addField :: Expr -> Module -> Module
addField field mod = mod { fields = field : fields mod }

addClass :: Expr -> Module -> Module
addClass ncls mod = mod { classes = ncls : classes mod }


addImport :: Expr -> Module -> Module
addImport imp mod = mod { imports = imp : imports mod }





traverseM :: Traversal m => (Expr -> m Expr) -> (Type -> m Type) -> (Pat -> m Pat) -> (Lit -> m Lit) -> Module -> m Module
traverseM fexp ftype fpat flit expr = case expr of
    Module     id cls imports classes             
               fields methods modules        -> Module     id      <$> ftype cls <*> fexpMap imports <*> fexpMap classes <*> fexpMap fields <*> fexpMap methods <*> fexpMap modules
    where fexpMap = mapM fexp

traverseM_ :: Traversal m => (Expr -> m a) -> (Type -> m b) -> (Pat -> m c) -> (Lit -> m d) -> Module -> m ()
traverseM_ fexp ftype fpat flit expr = case expr of
    Module     id cls imports classes             
               fields methods modules        -> drop <* ftype cls <* fexpMap imports <* fexpMap classes <* fexpMap fields <* fexpMap methods <* fexpMap modules
    where drop    = pure ()
          fexpMap = mapM_ fexp


traverseM' :: Traversal m => (Expr -> m Expr) -> Module -> m Module
traverseM' fexp expr = traverseM fexp pure pure pure expr


traverseM'_ :: Traversal m => (Expr -> m ()) -> Module -> m ()
traverseM'_ fexp expr = traverseM_ fexp pure pure pure expr