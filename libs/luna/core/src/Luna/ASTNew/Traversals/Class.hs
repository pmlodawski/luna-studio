---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
 {-# LANGUAGE UndecidableInstances #-}

module Luna.ASTNew.Traversals.Class where

import           Flowbox.Prelude        hiding (Cons, Traversal, traverse)
import           GHC.Generics           (Generic)
import           Luna.ASTNew.Decl       (Decl, LDecl, Cons, ImpTgt)
import qualified Luna.ASTNew.Decl       as Decl
import           Luna.ASTNew.Module     (Module)
import qualified Luna.ASTNew.Module     as Module
import           Luna.ASTNew.Label      (Label(Label))
import qualified Luna.ASTNew.Label      as Label
import           Luna.ASTNew.Arg        (Arg)
import           Luna.ASTNew.Type       (Type)
import           Luna.ASTNew.Name       (TName, VName, CName, TVName)
import           Luna.ASTNew.Name.Multi (MultiName)
import           Luna.ASTNew.Native     (Native)

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class Traversal base m a where
    traverse :: (Monad m, Applicative m) => base -> a -> m a


class DefaultTraversal base m a where
    defaultTraverse :: (Monad m, Applicative m) => base -> a -> m a

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

------- basic types -----

instance (Monad m, Traversal base m a) => Traversal base m [a] where
    traverse base a = mapM (traverse base) a

instance Traversal base m a => Traversal base m (Maybe a) where
    traverse base = \case
        Just a  -> fmap Just $ traverse base a
        Nothing -> pure Nothing


------- basic AST types -----

instance Traversal base m TName     where traverse _ = pure
instance Traversal base m VName     where traverse _ = pure
instance Traversal base m CName     where traverse _ = pure
instance Traversal base m TVName    where traverse _ = pure
instance Traversal base m MultiName where traverse _ = pure


------- Label -----
instance Traversal base m a => Traversal base m (Label l a) where
    traverse base (Label l a) = fmap (Label l) $ traverse base a


------- Module -----

instance Traversal base m (Decl f e) => DefaultTraversal base m (Module f e) where
    defaultTraverse b (Module.Module path name body) = Module.Module <$> traverse b path <*> traverse b name <*> traverse b body


------- Decl -----

instance ( Traversal base m (Decl f e)
         , Traversal base m (Cons f e)
         , Traversal base m (Arg  f e)
         , Traversal base m (Type f)
         , Traversal base m (Native (LDecl f e))
         , Traversal base m ImpTgt
         , Traversal base m e
         ) => DefaultTraversal base m (Decl f e) where
    defaultTraverse b d = case d of
        Decl.Data        name params cons defs        -> Decl.Data        <$> traverse b name <*> traverse b params <*> traverse b cons    <*> traverse b defs
        Decl.Function    path name inputs output body -> Decl.Function    <$> traverse b path <*> traverse b name   <*> traverse b inputs  <*> traverse b output <*> traverse b body
        Decl.Import      path rename targets          -> Decl.Import      <$> traverse b path <*> traverse b rename <*> traverse b targets
        Decl.TypeAlias   dst src                      -> Decl.TypeAlias   <$> traverse b dst  <*> traverse b src
        Decl.TypeWrapper dst src                      -> Decl.TypeWrapper <$> traverse b dst  <*> traverse b src
        Decl.Native      nat                          -> Decl.Native      <$> traverse b nat


