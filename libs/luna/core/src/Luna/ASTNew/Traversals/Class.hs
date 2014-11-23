---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
 {-# LANGUAGE NoMonomorphismRestriction #-}
 {-# LANGUAGE UndecidableInstances #-}
 {-# LANGUAGE OverlappingInstances #-}
 {-# LANGUAGE FunctionalDependencies #-}
 {-# LANGUAGE GADTs #-}

  {-# LANGUAGE DysfunctionalDependencies #-}

module Luna.ASTNew.Traversals.Class where

import           Flowbox.Prelude        hiding (Cons, Traversal, traverse)
import           GHC.Generics           (Generic)
import           Control.Monad.Identity (runIdentity)

import           Luna.ASTNew.Decl       (Decl, LDecl, LCons, ImpTgt)
import qualified Luna.ASTNew.Decl       as Decl
import           Luna.ASTNew.Module     (Module(Module))
import qualified Luna.ASTNew.Module     as Module
import           Luna.ASTNew.Unit       (Unit(Unit))
import           Luna.ASTNew.Arg        (LArg, Arg(Arg))
import qualified Luna.ASTNew.Type       as Type
import           Luna.ASTNew.Type       (LType, Type)
import           Luna.ASTNew.Name       (TName, VName, CName, TVName)
import           Luna.ASTNew.Name.Multi (MultiName)
import           Luna.ASTNew.Native     (Native)
import           Luna.ASTNew.Label      (Label(Label))
import qualified Luna.ASTNew.Pat        as Pat
import           Luna.ASTNew.Pat        (Pat, LPat)
import           Luna.ASTNew.Lit        (LLit, Lit)
import qualified Luna.ASTNew.Expr       as Expr
import           Luna.ASTNew.Expr       (LExpr, Expr)
import           Luna.ASTNew.Name       (NameBase)

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

--type MonoTraversal base m a = Traversal base m a a
--type DropTraversal base m a = Traversal base m a ()


class Traversal base m a b | base a -> b where
    traverseM :: (Monad m, Applicative m) => base -> a -> m b

class Traversal2 base m a b | base a -> b where
    traverseM2 :: (Monad m, Applicative m) => base -> a -> m b

class MonoTraversal2 base m a where
    monoTraverseM :: (Monad m, Applicative m) => base -> a -> m a


class DefaultTraversal base m a b | base a -> b where
    defaultTraverseM :: (Monad m, Applicative m) => base -> a -> m b

type MonoTraversal base m a = Traversal base m a a

defaultMonoTraverseM :: (DefaultTraversal base m a a, Monad m, Applicative m) 
                     => base -> a -> m a
defaultMonoTraverseM = defaultTraverseM


--monoTraverseM :: (MonoTraversal base m a, Applicative m, Monad m) => base -> a -> m a
--monoTraverseM = traverseM

--dropTraverseM :: (DropTraversal base m a, Applicative m, Monad m) => base -> a -> m ()
--dropTraverseM = traverseM

traverse :: Traversal base Identity a b => base -> a -> b
traverse base = runIdentity . traverseM base

--monoTraverse :: Traversal base Identity a a => base -> a -> a
--monoTraverse = traverse


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

-- ----- basic types -----

instance (Monad m, Traversal base m a b) => Traversal base m [a] [b] where
    traverseM base a = mapM (traverseM base) a

instance (Monad m, Traversal base m a b) => DefaultTraversal base m [a] [b] where
    defaultTraverseM base a = mapM (traverseM base) a

instance Traversal base m a b => Traversal base m (Maybe a) (Maybe b) where
    traverseM base = \case
        Just a  -> fmap Just $ traverseM base a
        Nothing -> pure Nothing

instance Traversal base m a b => DefaultTraversal base m (Maybe a) (Maybe b) where
    defaultTraverseM base = \case
        Just a  -> fmap Just $ traverseM base a
        Nothing -> pure Nothing

instance Traversal        base m String String where traverseM        _ = pure
instance DefaultTraversal base m String String where defaultTraverseM _ = pure

-- ----- basic AST types -----

instance Traversal base m TName         TName         where traverseM _ = pure
instance Traversal base m VName         VName         where traverseM _ = pure
instance Traversal base m CName         CName         where traverseM _ = pure
instance Traversal base m TVName        TVName        where traverseM _ = pure
instance Traversal base m (MultiName a) (MultiName a) where traverseM _ = pure
instance Traversal base m NameBase      NameBase      where traverseM _ = pure


-- ----- Unit -----

instance Traversal base m a b => DefaultTraversal base m (Unit a) (Unit b) where
    defaultTraverseM b (Unit a) = Unit <$> traverseM b a


-- ----- Module -----

instance Traversal base m (LDecl f e) (LDecl f' e') => DefaultTraversal base m (Module f e) (Module f' e') where
    defaultTraverseM b (Module path name body) = Module <$> traverseM b path <*> traverseM b name <*> traverseM b body


-- ----- Decl -----

instance ( Traversal base m (LDecl a e)          (LDecl a' e')
         , Traversal base m (LCons a e)          (LCons a' e')
         , Traversal base m (Arg  a e)           (Arg   a' e')
         , Traversal base m (LType a)            (LType a'   )
         , Traversal base m (Native (LDecl a e)) (Native (LDecl a' e'))
         , Traversal base m ImpTgt               ImpTgt
         , Traversal base m e e'
         ) => DefaultTraversal base m (Decl a e) (Decl a' e') where
    defaultTraverseM b = \case
        Decl.Data        name params cons defs        -> Decl.Data        <$> traverseM b name <*> traverseM b params <*> traverseM b cons    <*> traverseM b defs
        Decl.Function    path name inputs output body -> Decl.Function    <$> traverseM b path <*> traverseM b name   <*> traverseM b inputs  <*> traverseM b output <*> traverseM b body
        Decl.Import      path rename targets          -> Decl.Import      <$> traverseM b path <*> traverseM b rename <*> traverseM b targets
        Decl.TypeAlias   dst src                      -> Decl.TypeAlias   <$> traverseM b dst  <*> traverseM b src
        Decl.TypeWrapper dst src                      -> Decl.TypeWrapper <$> traverseM b dst  <*> traverseM b src
        Decl.Native      nat                          -> Decl.Native      <$> traverseM b nat

instance Traversal base m (Decl.LField a e) (Decl.LField a' e') => DefaultTraversal base m (Decl.Cons a e) (Decl.Cons a' e') where
    defaultTraverseM b (Decl.Cons name fields) = Decl.Cons <$> traverseM b name <*> traverseM b fields

instance ( Traversal base m (LType a) (LType a')
         , Traversal base m e e') 
         => DefaultTraversal base m (Decl.Field a e) (Decl.Field a' e') where
    defaultTraverseM b (Decl.Field tp name val) = Decl.Field <$> traverseM b tp <*> traverseM b name <*> traverseM b val


instance DefaultTraversal base m Decl.ImpTgt Decl.ImpTgt where
    defaultTraverseM _ = pure


-- ----- Type -----

instance (Traversal base m (LType a) (LType a')) => DefaultTraversal base m (Type a) (Type a') where
    defaultTraverseM b = \case
        Type.Function inputs output -> Type.Function <$> traverseM b inputs <*> traverseM b output
        Type.App      src args      -> Type.App      <$> traverseM b src    <*> traverseM b args
        Type.Var      name          -> Type.Var      <$> traverseM b name
        Type.Tuple    items         -> Type.Tuple    <$> traverseM b items
        Type.List     item          -> Type.List     <$> traverseM b item
        Type.Con      segments      -> Type.Con      <$> traverseM b segments
        Type.Wildcard               -> pure Type.Wildcard


-- ----- Arg -----

instance ( Traversal base m (LPat a) (LPat a')
         , Traversal base m v v') 
         => DefaultTraversal base m (Arg a v) (Arg a' v') where
    defaultTraverseM b (Arg pat value) = Arg <$> traverseM b pat <*> traverseM b value



-- ----- Native -----

instance Traversal base m e e' => DefaultTraversal base m (Native e) (Native e') 


-- ----- Pat -----

instance ( Traversal base m (LPat a)  (LPat  a')
         , Traversal base m (LLit a)  (LLit  a')
         , Traversal base m (LType a) (LType a')
         ) => DefaultTraversal base m (Pat a) (Pat a') where
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


-- ----- Expr -----

instance ( Traversal base m v v'
         , Traversal base m (Native (LExpr lab v)) (Native (LExpr lab' v'))
         , Traversal base m (LLit lab) (LLit lab')
         , Traversal base m (Expr.SubDecl lab v) (Expr.SubDecl lab' v')
         , Traversal base m (Expr.LList lab (LExpr lab v)) (Expr.LList lab' (LExpr lab' v'))
         , Traversal base m (LPat lab) (LPat lab')
         , Traversal base m (Expr.LMatch lab v) (Expr.LMatch lab' v')
         , Traversal base m (Expr.ExprApp lab v) (Expr.ExprApp lab' v')
         , Traversal base m (LExpr lab v) (LExpr lab' v')
         , Traversal base m (LType lab) (LType lab')
         , Traversal base m (LArg lab (Expr lab v)) (LArg lab' (Expr lab' v'))
         ) => DefaultTraversal base m (Expr lab v) (Expr lab' v') where
    defaultTraverseM b = \case
        Expr.Lambda      inputs  output   body -> Expr.Lambda      <$> traverseM b inputs <*> traverseM b output   <*> traverseM b body
        Expr.RecUpdt     src     selector expr -> Expr.RecUpdt     <$> traverseM b src    <*> traverseM b selector <*> traverseM b expr
        Expr.App         src     args          -> Expr.App         <$> traverseM b src    <*> traverseM b args         
        Expr.Case        expr    match         -> Expr.Case        <$> traverseM b expr   <*> traverseM b match        
        Expr.Typed       cls     expr          -> Expr.Typed       <$> traverseM b cls    <*> traverseM b expr         
        Expr.Assignment  dst     src           -> Expr.Assignment  <$> traverseM b dst    <*> traverseM b src          
        Expr.Accessor    acc     src           -> Expr.Accessor    <$> traverseM b acc    <*> traverseM b src          
        Expr.Ref         ref                   -> Expr.Ref         <$> traverseM b ref                  
        Expr.List        elems                 -> Expr.List        <$> traverseM b elems                
        Expr.Tuple       items                 -> Expr.Tuple       <$> traverseM b items                
        Expr.Grouped     expr                  -> Expr.Grouped     <$> traverseM b expr                 
        Expr.Cons        cname                 -> Expr.Cons        <$> traverseM b cname                
        Expr.Decl        decl                  -> Expr.Decl        <$> traverseM b decl                 
        Expr.Lit         lit                   -> Expr.Lit         <$> traverseM b lit                  
        Expr.Native      native                -> Expr.Native      <$> traverseM b native               
        Expr.Var         ident                 -> Expr.Var         <$> traverseM b ident                
        Expr.Wildcard                          -> pure Expr.Wildcard                         


instance ( Traversal base m (LPat lab) (LPat lab')
         , Traversal base m (LExpr lab v) (LExpr lab' v')
         ) => DefaultTraversal base m (Expr.Match lab v) (Expr.Match lab' v') where
    defaultTraverseM b (Expr.Match pat body) = Expr.Match <$> traverseM b pat <*> traverseM b body


instance ( Traversal base m (Expr.Named VName a) (Expr.Named VName a')
         , Traversal base m a a'
         ) => DefaultTraversal base m (Expr.App a) (Expr.App a') where
    defaultTraverseM b = \case
        Expr.Seq   ops -> Expr.Seq   <$> traverseM b ops
        Expr.Infix l r -> Expr.Infix <$> traverseM b l <*> traverseM b r


instance ( Traversal base m n n'
         , Traversal base m v v'
         ) => DefaultTraversal base m (Expr.Named n v) (Expr.Named n' v') where
    defaultTraverseM b = \case
        Expr.Named   n v -> Expr.Named   <$> traverseM b n <*> traverseM b v
        Expr.Unnamed   v -> Expr.Unnamed <$> traverseM b v


instance ( Traversal base m e e'
         , Traversal base m (Expr.Sequence e) (Expr.Sequence e')
         ) => DefaultTraversal base m (Expr.List e) (Expr.List e') where
    defaultTraverseM b = \case
        Expr.SeqList   els -> Expr.SeqList   <$> traverseM b els
        Expr.RangeList s   -> Expr.RangeList <$> traverseM b s


instance (Traversal base m a a') => DefaultTraversal base m (Expr.Sequence a) (Expr.Sequence a') where
    defaultTraverseM b = \case
        Expr.Linear    a   ma -> Expr.Linear    <$> traverseM b a                   <*> traverseM b ma
        Expr.Geometric l r ma -> Expr.Geometric <$> traverseM b l <*> traverseM b r <*> traverseM b ma




---- ----- Lit -----

instance DefaultTraversal base m Lit Lit where defaultTraverseM _ = pure