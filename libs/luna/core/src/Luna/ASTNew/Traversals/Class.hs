---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
 {-# LANGUAGE NoMonomorphismRestriction #-}
 {-# LANGUAGE UndecidableInstances #-}
 {-# LANGUAGE OverlappingInstances #-}

module Luna.ASTNew.Traversals.Class where

import           Flowbox.Prelude        hiding (Cons, Traversal, traverse)
import           GHC.Generics           (Generic)
import           Control.Monad.Identity (runIdentity)

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

type MonoTraversal base m a = Traversal base m a a
type DropTraversal base m a = Traversal base m a ()

class Traversal base m a b where
    traverseM :: (Monad m, Applicative m) => base -> a -> m b


class DefaultTraversal base m a b where
    defaultTraverseM :: (Monad m, Applicative m) => base -> a -> m b


monoTraverseM :: (MonoTraversal base m a, Applicative m, Monad m) => base -> a -> m a
monoTraverseM = traverseM

dropTraverseM :: (DropTraversal base m a, Applicative m, Monad m) => base -> a -> m ()
dropTraverseM = traverseM

traverse :: Traversal base Identity a b => base -> a -> b
traverse base = runIdentity . traverseM base

monoTraverse :: Traversal base Identity a a => base -> a -> a
monoTraverse = traverse


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

-- ----- basic types -----

instance (Monad m, Traversal base m a b) => Traversal base m [a] [b] where
    traverseM base a = mapM (traverseM base) a

instance Traversal base m a b => Traversal base m (Maybe a) (Maybe b) where
    traverseM base = \case
        Just a  -> fmap Just $ traverseM base a
        Nothing -> pure Nothing

instance Traversal base m String String        where traverseM _ = pure
instance DefaultTraversal base m String String where defaultTraverseM _ = pure


-- ----- basic AST types -----

instance Traversal base m TName     TName     where traverseM _ = pure
instance Traversal base m VName     VName     where traverseM _ = pure
instance Traversal base m CName     CName     where traverseM _ = pure
instance Traversal base m TVName    TVName    where traverseM _ = pure
instance Traversal base m MultiName MultiName where traverseM _ = pure


-- ----- Unit -----

instance Traversal base m a b => DefaultTraversal base m (Unit a) (Unit b) where
    defaultTraverseM b (Unit a) = Unit <$> traverseM b a


-- ----- Module -----

instance Traversal base m (LDecl f e) (LDecl f e) => DefaultTraversal base m (Module f e) (Module f e) where
    defaultTraverseM b (Module path name body) = Module <$> traverseM b path <*> traverseM b name <*> traverseM b body


-- ----- Decl -----

instance ( Traversal base m (LDecl a e)          (LDecl a f)
         , Traversal base m (LCons a e)          (LCons a f)
         , Traversal base m (Arg  a e)           (Arg  a f)
         , Traversal base m (LType a)            (LType a)
         , Traversal base m (Native (LDecl a e)) (Native (LDecl a f))
         , Traversal base m ImpTgt               ImpTgt
         , Traversal base m e f
         ) => DefaultTraversal base m (Decl a e) (Decl a f) where
    defaultTraverseM b = \case
        Decl.Data        name params cons defs        -> Decl.Data        <$> traverseM b name <*> traverseM b params <*> traverseM b cons    <*> traverseM b defs
        Decl.Function    path name inputs output body -> Decl.Function    <$> traverseM b path <*> traverseM b name   <*> traverseM b inputs  <*> traverseM b output <*> traverseM b body
        Decl.Import      path rename targets          -> Decl.Import      <$> traverseM b path <*> traverseM b rename <*> traverseM b targets
        Decl.TypeAlias   dst src                      -> Decl.TypeAlias   <$> traverseM b dst  <*> traverseM b src
        Decl.TypeWrapper dst src                      -> Decl.TypeWrapper <$> traverseM b dst  <*> traverseM b src
        Decl.Native      nat                          -> Decl.Native      <$> traverseM b nat

instance Traversal base m (Decl.LField a e) (Decl.LField a e) => DefaultTraversal base m (Decl.Cons a e) (Decl.Cons a e) where
    defaultTraverseM b (Decl.Cons name fields) = Decl.Cons <$> traverseM b name <*> traverseM b fields

instance ( Traversal base m (LType a) (LType a)
         , Traversal base m e f) 
         => DefaultTraversal base m (Decl.Field a e) (Decl.Field a f) where
    defaultTraverseM b (Decl.Field tp name val) = Decl.Field <$> traverseM b tp <*> traverseM b name <*> traverseM b val


instance DefaultTraversal base m Decl.ImpTgt Decl.ImpTgt where
    defaultTraverseM _ = pure


-- ----- Type -----

instance (Traversal base m (LType a) (LType a)) => DefaultTraversal base m (Type a) (Type a) where
    defaultTraverseM b = \case
        Type.Function inputs output -> Type.Function <$> traverseM b inputs <*> traverseM b output
        Type.App      src args      -> Type.App      <$> traverseM b src    <*> traverseM b args
        Type.Var      name          -> Type.Var      <$> traverseM b name
        Type.Tuple    items         -> Type.Tuple    <$> traverseM b items
        Type.List     item          -> Type.List     <$> traverseM b item
        Type.Con      segments      -> Type.Con      <$> traverseM b segments
        Type.Wildcard               -> pure Type.Wildcard


-- ----- Arg -----

instance ( Traversal base m (LPat a) (LPat a)
         , Traversal base m v g) 
         => DefaultTraversal base m (Arg a v) (Arg a g) where
    defaultTraverseM b (Arg pat value) = Arg <$> traverseM b pat <*> traverseM b value



-- ----- Native -----

instance DefaultTraversal base m (Native e) (Native e) 


-- ----- Pat -----

instance ( Traversal base m (LPat a)  (LPat a)
         , Traversal base m (LLit a)  (LLit a)
         , Traversal base m (LType a) (LType a)
         ) => DefaultTraversal base m (Pat a) (Pat a) where
    defaultTraverseM b = \case
        Pat.App         src   args -> Pat.App         <$> traverseM b src   <*> traverseM b args
        Pat.Typed       pat   cls  -> Pat.Typed       <$> traverseM b pat   <*> traverseM b cls
        Pat.Grouped     pat        -> Pat.Grouped     <$> traverseM b pat 
        Pat.Lit         lit        -> Pat.Lit         <$> traverseM b lit 
        Pat.Tuple       items      -> Pat.Tuple       <$> traverseM b items 
        Pat.Con         name       -> Pat.Con         <$> traverseM b name
        Pat.Var         name       -> Pat.Var         <$> traverseM b name
        Pat.Wildcard               -> pure Pat.Wildcard   
        Pat.RecWildcard            -> pure Pat.RecWildcard



---- ----- Lit -----

instance DefaultTraversal base m Lit Lit where defaultTraverseM _ = pure