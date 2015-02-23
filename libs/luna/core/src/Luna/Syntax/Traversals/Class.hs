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

module Luna.Syntax.Traversals.Class where

import Flowbox.Prelude hiding (Cons, Traversal, traverse)

import Control.Monad.Identity (runIdentity)

import           Luna.Syntax.Decl         (Decl, LDecl, LCons, ImpTgt)
import qualified Luna.Syntax.Decl         as Decl
import           Luna.Syntax.Module       (Module(Module))
import qualified Luna.Syntax.Module       as Module
import           Luna.Syntax.Unit         (Unit(Unit))
import           Luna.Syntax.Arg          (LArg, Arg(Arg))
import qualified Luna.Syntax.Type         as Type
import           Luna.Syntax.Type         (LType, Type, LMeta, Meta)
import           Luna.Syntax.Name         (TName, VName, CName, TVName)
import           Luna.Syntax.Name.Path    (NamePath, QualPath)
import           Luna.Syntax.Native       (Native)
import           Luna.Syntax.Label        (Label(Label))
import qualified Luna.Syntax.Pat          as Pat
import           Luna.Syntax.Pat          (Pat, LPat)
import           Luna.Syntax.Lit          (LLit, Lit)
import qualified Luna.Syntax.Expr         as Expr
import           Luna.Syntax.Expr         (LExpr, Expr)
import           Luna.Syntax.Name         (NameBase)
import           Luna.Syntax.Name.Pattern (NamePat(NamePat), Segment(Segment), SegmentName)
import qualified Luna.Syntax.Name.Pattern as Pattern
import           Luna.Syntax.Foreign      (Foreign(Foreign))
import qualified Luna.Syntax.Foreign      as Foreign
import           Luna.Syntax.Pragma       (Pragma)
import qualified Luna.Syntax.Pragma       as Pragma

import Data.Text.Lazy           (Text)

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

instance ( Traversal base m arg arg'
         , Traversal base m (Segment SegmentName arg) (Segment SegmentName arg')
         , Traversal base m (Segment sbase arg) (Segment sbase' arg')
         ) => DefaultTraversal base m (NamePat sbase arg) (NamePat sbase' arg') where
    defaultTraverseM b (NamePat prefix base segments) = NamePat <$> traverseM b prefix <*> traverseM b base <*> traverseM b segments


instance ( Traversal base m arg arg'
         , Traversal base m sbase sbase'
         ) => DefaultTraversal base m (Segment sbase arg) (Segment sbase' arg') where
    defaultTraverseM b (Segment base args) = Segment <$> traverseM b base <*> traverseM b args


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

instance Traversal        base m ()     ()     where traverseM _ = pure
instance Traversal        base m Char   Char   where traverseM _ = pure
instance Traversal        base m Text   Text   where traverseM _ = pure
--instance DefaultTraversal base m String String where defaultTraverseM _ = pure

-- ----- basic AST types -----

instance Traversal base m (TName    a) (TName    a) where traverseM _ = pure
instance Traversal base m (VName    a) (VName    a) where traverseM _ = pure
instance Traversal base m (CName    a) (CName    a) where traverseM _ = pure
instance Traversal base m (TVName   a) (TVName   a) where traverseM _ = pure
instance Traversal base m (NameBase a) (NameBase a) where traverseM _ = pure
instance Traversal base m NamePath     NamePath     where traverseM _ = pure


-- ----- Unit -----

instance Traversal base m a b => DefaultTraversal base m (Unit a) (Unit b) where
    defaultTraverseM b (Unit a) = Unit <$> traverseM b a


-- ----- Module -----

instance ( Traversal base m (LDecl f e) (LDecl f' e')
         , Traversal base m QualPath QualPath
         ) => DefaultTraversal base m (Module f e) (Module f' e') where
    defaultTraverseM b (Module path body) = Module <$> traverseM b path <*> traverseM b body


instance DefaultTraversal base m QualPath QualPath where
    defaultTraverseM _ = pure

-- ----- Decl -----

instance ( Traversal base m (LDecl a e)          (LDecl a' e')
         , Traversal base m (LCons a e)          (LCons a' e')
         , Traversal base m (Arg  a e)           (Arg   a' e')
         , Traversal base m (LType a)            (LType a'   )
         , Traversal base m (Decl.FuncSig a e)   (Decl.FuncSig a' e')
         , Traversal base m e e'
         , Traversal base m (Decl.DataDecl a e) (Decl.DataDecl a' e')
         , Traversal base m (Decl.FuncDecl a e [e]) (Decl.FuncDecl a' e' [e'])
         , Traversal base m (Foreign (Decl.ForeignDecl a e)) (Foreign (Decl.ForeignDecl a' e'))
         , Traversal base m Pragma Pragma
         , Traversal base m Decl.Imp Decl.Imp
         ) => DefaultTraversal base m (Decl a e) (Decl a' e') where
    defaultTraverseM b = \case
        Decl.Data        ddecl                 -> Decl.Data        <$> traverseM b ddecl
        Decl.Func        fdecl                 -> Decl.Func        <$> traverseM b fdecl
        Decl.Imp         imp                   -> Decl.Imp         <$> traverseM b imp
        Decl.TpAls       dst src               -> Decl.TpAls       <$> traverseM b dst  <*> traverseM b src
        Decl.TpWrp       dst src               -> Decl.TpWrp       <$> traverseM b dst  <*> traverseM b src
        Decl.Foreign     fdecl                 -> Decl.Foreign     <$> traverseM b fdecl
        Decl.Pragma      pragma                -> Decl.Pragma      <$> traverseM b pragma


instance Traversal base m (Decl.LField a e) (Decl.LField a' e') => DefaultTraversal base m (Decl.Cons a e) (Decl.Cons a' e') where
    defaultTraverseM b (Decl.Cons name fields) = Decl.Cons <$> traverseM b name <*> traverseM b fields

instance ( Traversal base m (LType a) (LType a')
         , Traversal base m e e') 
         => DefaultTraversal base m (Decl.Field a e) (Decl.Field a' e') where
    defaultTraverseM b (Decl.Field tp name val) = Decl.Field <$> traverseM b tp <*> traverseM b name <*> traverseM b val


instance ( Traversal base m (Label a (Decl.Cons a e)) (Label a' (Decl.Cons a' e'))
         , Traversal base m (Label a (Decl a e)) (Label a' (Decl a' e'))
         ) => DefaultTraversal base m (Decl.DataDecl a e) (Decl.DataDecl a' e') where
    defaultTraverseM b (Decl.DataDecl name params cons defs) = Decl.DataDecl <$> traverseM b name <*> traverseM b params <*> traverseM b cons <*> traverseM b defs


instance ( Traversal base m (Decl.FuncSig a e) (Decl.FuncSig a' e')
         , Traversal base m (Label a (Type a)) (Label a' (Type a'))
         , Traversal base m body body'
         ) => DefaultTraversal base m (Decl.FuncDecl a e body) (Decl.FuncDecl a' e' body') where
    defaultTraverseM b (Decl.FuncDecl path sig output body) = Decl.FuncDecl <$> traverseM b path <*> traverseM b sig <*> traverseM b output <*> traverseM b body


instance Traversal base m ImpTgt ImpTgt => DefaultTraversal base m Decl.Imp Decl.Imp where
    defaultTraverseM b = \case
        Decl.ModImp  path rename -> Decl.ModImp  <$> traverseM b path <*> traverseM b rename
        Decl.DeclImp path tgts   -> Decl.DeclImp <$> traverseM b path <*> traverseM b tgts

instance DefaultTraversal base m ImpTgt ImpTgt where
    defaultTraverseM _ = pure


instance ( Traversal base m (Decl.DataDecl a e) (Decl.DataDecl a' e')
         , Traversal base m (Decl.FuncDecl a e Decl.ForeignCode) (Decl.FuncDecl a' e' Decl.ForeignCode)
         ) => DefaultTraversal base m (Decl.ForeignDecl a e) (Decl.ForeignDecl a' e') where
    defaultTraverseM b = \case
        Decl.FData ddecl -> Decl.FData <$> traverseM b ddecl
        Decl.FFunc fdecl -> Decl.FFunc <$> traverseM b fdecl
        Decl.FImp  fimp  -> pure $ Decl.FImp fimp

-- ----- Pragma -----

instance DefaultTraversal base m Pragma Pragma where
    defaultTraverseM _ = pure

-- ----- Foreign -----



instance ( Traversal base m Foreign.Target Foreign.Target
         , Traversal base m a a'
         ) => DefaultTraversal base m (Foreign a) (Foreign a') where
    defaultTraverseM b (Foreign target a) = Foreign <$> traverseM b target <*> traverseM b a


instance DefaultTraversal base m (Foreign.Target) (Foreign.Target) where
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


instance DefaultTraversal base m Meta Meta where
    defaultTraverseM _ = pure

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
         , Traversal base m (Expr.List (LExpr lab v)) (Expr.List (LExpr lab' v'))
         , Traversal base m (LPat lab) (LPat lab')
         , Traversal base m (Expr.LMatch lab v) (Expr.LMatch lab' v')
         , Traversal base m (LExpr lab v) (LExpr lab' v')
         , Traversal base m (LType lab) (LType lab')
         , Traversal base m (LArg lab (LExpr lab v)) (LArg lab' (LExpr lab' v'))
         , Traversal base m (NamePat (LExpr lab v) (Expr.AppArg (LExpr lab v))) (NamePat (LExpr lab' v') (Expr.AppArg (LExpr lab' v')))
         , Traversal base m (Expr.Variable v) (Expr.Variable v')
         , Traversal base m (Expr.FieldUpd lab v) (Expr.FieldUpd lab' v')
         , Traversal base m (LMeta lab) (LMeta lab')
         ) => DefaultTraversal base m (Expr lab v) (Expr lab' v') where
    defaultTraverseM b = \case
        Expr.Lambda      inputs  output   body -> Expr.Lambda      <$> traverseM b inputs <*> traverseM b output   <*> traverseM b body
        Expr.RecUpd      src     fupds         -> Expr.RecUpd      <$> traverseM b src    <*> traverseM b fupds
        Expr.App         app                   -> Expr.App         <$> traverseM b app
        Expr.Case        expr    match         -> Expr.Case        <$> traverseM b expr   <*> traverseM b match        
        Expr.Typed       cls     expr          -> Expr.Typed       <$> traverseM b cls    <*> traverseM b expr         
        Expr.Assignment  dst     src           -> Expr.Assignment  <$> traverseM b dst    <*> traverseM b src          
        Expr.Accessor    acc     src           -> Expr.Accessor    <$> traverseM b acc    <*> traverseM b src          
        Expr.Curry       ref                   -> Expr.Curry       <$> traverseM b ref                  
        Expr.Meta        meta                  -> Expr.Meta        <$> traverseM b meta
        Expr.List        elems                 -> Expr.List        <$> traverseM b elems                
        Expr.Tuple       items                 -> Expr.Tuple       <$> traverseM b items                
        Expr.Grouped     expr                  -> Expr.Grouped     <$> traverseM b expr                 
        Expr.Cons        cname                 -> Expr.Cons        <$> traverseM b cname                
        Expr.Decl        decl                  -> Expr.Decl        <$> traverseM b decl                 
        Expr.Lit         lit                   -> Expr.Lit         <$> traverseM b lit                  
        Expr.Native      native                -> Expr.Native      <$> traverseM b native               
        Expr.Var         ident                 -> Expr.Var         <$> traverseM b ident                
        Expr.Wildcard                          -> pure Expr.Wildcard                         


instance DefaultTraversal base m (Expr.Variable e) (Expr.Variable e) where
    defaultTraverseM _ = pure


instance Traversal base m (LExpr a v) (LExpr a' v')
         => DefaultTraversal base m (Expr.FieldUpd a v) (Expr.FieldUpd a' v') where
    defaultTraverseM b (Expr.FieldUpd sel expr) = Expr.FieldUpd <$> traverseM b sel <*> traverseM b expr


instance Traversal base m e e'
         => DefaultTraversal base m (Expr.AppArg e) (Expr.AppArg e') where
    defaultTraverseM b (Expr.AppArg name e) = Expr.AppArg <$> traverseM b name <*> traverseM b e


instance ( Traversal base m (LPat lab) (LPat lab')
         , Traversal base m (LExpr lab v) (LExpr lab' v')
         ) => DefaultTraversal base m (Expr.Match lab v) (Expr.Match lab' v') where
    defaultTraverseM b (Expr.Match pat body) = Expr.Match <$> traverseM b pat <*> traverseM b body


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