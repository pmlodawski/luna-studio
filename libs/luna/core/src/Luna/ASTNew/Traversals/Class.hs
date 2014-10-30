---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
 {-# LANGUAGE UndecidableInstances #-}
 {-# LANGUAGE OverlappingInstances #-}

module Luna.ASTNew.Traversals.Class where

import           Flowbox.Prelude        hiding (Cons, Traversal, traverse)
import           GHC.Generics           (Generic)
import           Luna.ASTNew.Decl       (Decl, LDecl, LCons, ImpTgt)
import qualified Luna.ASTNew.Decl       as Decl
import           Luna.ASTNew.Module     (Module(Module))
import qualified Luna.ASTNew.Module     as Module
import           Luna.ASTNew.Unit       (Unit(Unit))
import           Luna.ASTNew.Arg        (Arg(Arg))
import qualified Luna.ASTNew.Type       as Type
import           Luna.ASTNew.Type       (LType, Type)
import           Luna.ASTNew.Name       (TName, VName, CName, TVName)
import           Luna.ASTNew.Name.Multi (MultiName)
import           Luna.ASTNew.Native     (Native)
import           Luna.ASTNew.Label      (Label(Label))
import qualified Luna.ASTNew.Pat        as Pat
import           Luna.ASTNew.Pat        (Pat, LPat)
import           Luna.ASTNew.Lit        (LLit, Lit)

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

-- ----- basic types -----

instance (Monad m, Traversal base m a) => Traversal base m [a] where
    traverse base a = mapM (traverse base) a

instance Traversal base m a => Traversal base m (Maybe a) where
    traverse base = \case
        Just a  -> fmap Just $ traverse base a
        Nothing -> pure Nothing


-- ----- basic AST types -----

instance Traversal base m String    where traverse _ = pure
instance DefaultTraversal base m String    where defaultTraverse _ = pure



instance Traversal base m TName     where traverse _ = pure
instance Traversal base m VName     where traverse _ = pure
instance Traversal base m CName     where traverse _ = pure
instance Traversal base m TVName    where traverse _ = pure
instance Traversal base m MultiName where traverse _ = pure


-- ----- Unit -----

instance Traversal base m a => DefaultTraversal base m (Unit a) where
    defaultTraverse b (Unit a) = Unit <$> traverse b a


-- ----- Module -----

instance Traversal base m (LDecl f e) => DefaultTraversal base m (Module f e) where
    defaultTraverse b (Module path name body) = Module <$> traverse b path <*> traverse b name <*> traverse b body





-- ----- Decl -----

instance ( Traversal base m (LDecl f e)
         , Traversal base m (LCons f e)
         , Traversal base m (Arg  f e)
         , Traversal base m (LType f)
         , Traversal base m (Native (LDecl f e))
         , Traversal base m ImpTgt
         , Traversal base m e
         ) => DefaultTraversal base m (Decl f e) where
    defaultTraverse b = \case
        Decl.Data        name params cons defs        -> Decl.Data        <$> traverse b name <*> traverse b params <*> traverse b cons    <*> traverse b defs
        Decl.Function    path name inputs output body -> Decl.Function    <$> traverse b path <*> traverse b name   <*> traverse b inputs  <*> traverse b output <*> traverse b body
        Decl.Import      path rename targets          -> Decl.Import      <$> traverse b path <*> traverse b rename <*> traverse b targets
        Decl.TypeAlias   dst src                      -> Decl.TypeAlias   <$> traverse b dst  <*> traverse b src
        Decl.TypeWrapper dst src                      -> Decl.TypeWrapper <$> traverse b dst  <*> traverse b src
        Decl.Native      nat                          -> Decl.Native      <$> traverse b nat

instance Traversal base m (Decl.LField a e) => DefaultTraversal base m (Decl.Cons a e) where
    defaultTraverse b (Decl.Cons name fields) = Decl.Cons <$> traverse b name <*> traverse b fields

instance ( Traversal base m (LType a)
         , Traversal base m e) 
         => DefaultTraversal base m (Decl.Field a e) where
    defaultTraverse b (Decl.Field tp name val) = Decl.Field <$> traverse b tp <*> traverse b name <*> traverse b val


instance DefaultTraversal base m Decl.ImpTgt where
    defaultTraverse _ = pure


-- ----- Type -----

instance (Traversal base m (LType a)) => DefaultTraversal base m (Type a) where
    defaultTraverse b = \case
        Type.Function inputs output -> Type.Function <$> traverse b inputs <*> traverse b output
        Type.App      src args      -> Type.App      <$> traverse b src    <*> traverse b args
        Type.Var      name          -> Type.Var      <$> traverse b name
        Type.Tuple    items         -> Type.Tuple    <$> traverse b items
        Type.List     item          -> Type.List     <$> traverse b item
        Type.Con      segments      -> Type.Con      <$> traverse b segments
        Type.Wildcard               -> pure Type.Wildcard


-- ----- Arg -----

instance ( Traversal base m (LPat a)
         , Traversal base m v) 
         => DefaultTraversal base m (Arg a v) where
    defaultTraverse b (Arg pat value) = Arg <$> traverse b pat <*> traverse b value



-- ----- Native -----

instance DefaultTraversal base m (Native e) 


-- ----- Pat -----

instance ( Traversal base m (LPat a)
         , Traversal base m (LLit a)
         , Traversal base m (LType a)) 
         => DefaultTraversal base m (Pat a) where
    defaultTraverse b = \case
        Pat.App         src   args -> Pat.App         <$> traverse b src   <*> traverse b args
        Pat.Typed       pat   cls  -> Pat.Typed       <$> traverse b pat   <*> traverse b cls
        Pat.Grouped     pat        -> Pat.Grouped     <$> traverse b pat 
        Pat.Lit         lit        -> Pat.Lit         <$> traverse b lit 
        Pat.Tuple       items      -> Pat.Tuple       <$> traverse b items 
        Pat.Con         name       -> Pat.Con         <$> traverse b name
        Pat.Var         name       -> Pat.Var         <$> traverse b name
        Pat.Wildcard               -> pure Pat.Wildcard   
        Pat.RecWildcard            -> pure Pat.RecWildcard



-- ----- Lit -----

instance DefaultTraversal base m Lit where defaultTraverse _ = pure