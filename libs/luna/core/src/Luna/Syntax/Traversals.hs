---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DysfunctionalDependencies #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Traversals (module X) where

import           Luna.Syntax.Traversals.Class   as X
import           Luna.Syntax.Traversals.Default as X

--import           Flowbox.Prelude        hiding (Traversal)
--import           Control.Applicative

--import           Luna.Syntax.Decl       (Decl, LDecl, LCons, ImpTgt)
--import qualified Luna.Syntax.Decl       as Decl
--import           Luna.Syntax.Module     (Module(Module))
--import qualified Luna.Syntax.Module     as Module
--import           Luna.Syntax.Unit       (Unit(Unit))
--import           Luna.Syntax.Arg        (Arg(Arg))
--import qualified Luna.Syntax.Type       as Type
--import           Luna.Syntax.Type       (LType, Type)
--import           Luna.Syntax.Expr       (Expr)
--import           Luna.Syntax.Name       (TName, VName, CName, TVName)
--import           Luna.Syntax.Name.Multi (MultiName)
--import           Luna.Syntax.Native     (Native)
--import           Luna.Syntax.Label      (Label(Label))
--import qualified Luna.Syntax.Pat        as Pat
--import           Luna.Syntax.Pat        (Pat, LPat)
--import           Luna.Syntax.Lit        (LLit, Lit)

--data Conf a e v a' e' v' m
--   = Conf { decl :: Label a (Decl a e) -> m (Label a' (Decl a' e' ))
--          , expr :: Label a (Expr a v) -> m (Label a' (Expr a' v'))
--          , lit  :: Label a (Lit     ) -> m (Label a' Lit)
--          , pat  :: Label a (Pat  a  ) -> m (Label a' (Pat  a'))
--          , tp   :: Label a (Type a  ) -> m (Label a' (Type a'))
--          }

--class Traversal cfg a m b | cfg a -> b where
--    traverseM :: (Monad m, Applicative m) => cfg m -> a -> m b

--traverseDecl cfg (Label l d) = fmap (Label l) $ case d of
--    Decl.Data        name params cons defs        -> Decl.Data        <$> traverseM cfg name <*> traverseM cfg params <*> traverseM cfg cons    <*> traverseM cfg defs
--    Decl.Function    path name inputs output body -> Decl.Function    <$> traverseM cfg path <*> traverseM cfg name   <*> traverseM cfg inputs  <*> traverseM cfg output <*> traverseM cfg body
--    Decl.Import      path rename targets          -> Decl.Import      <$> traverseM cfg path <*> traverseM cfg rename <*> traverseM cfg targets
--    Decl.TypeAlias   dst src                      -> Decl.TypeAlias   <$> traverseM cfg dst  <*> traverseM cfg src
--    Decl.TypeWrapper dst src                      -> Decl.TypeWrapper <$> traverseM cfg dst  <*> traverseM cfg src
--    Decl.Native      nat                          -> Decl.Native      <$> traverseM cfg nat


--traverseType cfg = \case
--    Type.Function inputs output -> Type.Function <$> traverseM cfg inputs <*> traverseM cfg output
--    Type.App      src args      -> Type.App      <$> traverseM cfg src    <*> traverseM cfg args
--    Type.Var      name          -> Type.Var      <$> traverseM cfg name
--    Type.Tuple    items         -> Type.Tuple    <$> traverseM cfg items
--    Type.List     item          -> Type.List     <$> traverseM cfg item
--    Type.Con      segments      -> Type.Con      <$> traverseM cfg segments
--    Type.Wildcard               -> pure Type.Wildcard


--instance (a1~a2, e1~e2) => Traversal (Conf a1 e1 v1 a' e' v') (Label a1 (Decl a2 e2)) m (Label a' (Decl a' e')) where traverseM = decl
--instance (a1~a2, v1~v2) => Traversal (Conf a1 e1 v1 a' e' v') (Label a1 (Expr a2 v2)) m (Label a' (Expr a' v')) where traverseM = expr
--instance (a1~a2)        => Traversal (Conf a1 e1 v1 a' e' v') (Label a1 (Pat  a2)   ) m (Label a' (Pat  a')   ) where traverseM = pat
--instance (a1~a2)        => Traversal (Conf a1 e1 v1 a' e' v') (Label a1 (Type a2)   ) m (Label a' (Type a')   ) where traverseM = tp
--instance Traversal  (Conf a1 e1 v1 a' e' v') (Label a1 Lit)        m (Label a' Lit)          where traverseM = lit
--instance Traversal  (Conf a1 e1 v1 a' e' v') TName      m TName        where traverseM _ = pure
--instance Traversal  (Conf a1 e1 v1 a' e' v') TVName     m TVName       where traverseM _ = pure
--instance Traversal  (Conf a1 e1 v1 a' e' v') ImpTgt     m ImpTgt       where traverseM _ = pure
--instance Traversal  (Conf a1 e1 v1 a' e' v') (Arg x y)  m (Arg x y)    where traverseM _ = pure
--instance Traversal  (Conf a1 e1 v1 a' e' v') Decl.Name  m Decl.Name    where traverseM _ = pure
--instance Traversal  (Conf a1 e1 v1 a' e' v') (Maybe x)  m (Maybe x)    where traverseM _ = pure
--instance (Traversal (Conf a1 e1 v1 a' e' v') x m x') => Traversal (Conf a1 e1 v1 a' e' v') [x] m [x'] where traverseM cfg = mapM (traverseM cfg)
--instance Traversal  (Conf a1 e1 v1 a' e' v') (Native x) m (Native x)   where traverseM _ = pure
----instance Traversal (Conf m a1 e1 v1 a' e' v') x m x' => Traversal (Conf m a1 e1 v1 a' e' v') (Label l x) m (Label l x')  where traverseM cfg (Label l x) = (fmap (Label l) $ traverseM cfg x)

--defcfg = Conf { decl = traverseDecl defcfg }

--instance Default (Conf m a e v a' e' v') where
--    def = defcfg

------------------------------------------------------------------------
---- Type classes
------------------------------------------------------------------------

--type MonoTraversal base m a = Traversal base m a a
--type DropTraversal base m a = Traversal base m a ()

--class Traversal base m a b where
--    traverseM :: (Monad m, Applicative m) => base -> a -> m b


--class DefaultTraversal base m a b where
--    defaultTraverseM :: (Monad m, Applicative m) => base -> a -> m b


--monoTraverseM :: (MonoTraversal base m a, Applicative m, Monad m) => base -> a -> m a
--monoTraverseM = traverseM

--dropTraverseM :: (DropTraversal base m a, Applicative m, Monad m) => base -> a -> m ()
--dropTraverseM = traverseM

--traverse :: Traversal base Identity a b => base -> a -> b
--traverse base = runIdentity . traverseM base

--monoTraverse :: Traversal base Identity a a => base -> a -> a
--monoTraverse = traverse


------------------------------------------------------------------------
---- Instances
------------------------------------------------------------------------

---- ----- basic types -----

--instance (Monad m, Traversal base m a b) => Traversal base m [a] [b] where
--    traverseM base a = mapM (traverseM base) a

--instance Traversal base m a b => Traversal base m (Maybe a) (Maybe b) where
--    traverseM base = \case
--        Just a  -> fmap Just $ traverseM base a
--        Nothing -> pure Nothing

--instance Traversal base m String String        where traverseM _ = pure
--instance DefaultTraversal base m String String where defaultTraverseM _ = pure


---- ----- basic AST types -----

--instance Traversal base m TName         TName         where traverseM _ = pure
--instance Traversal base m VName         VName         where traverseM _ = pure
--instance Traversal base m CName         CName         where traverseM _ = pure
--instance Traversal base m TVName        TVName        where traverseM _ = pure
--instance Traversal base m (MultiName a) (MultiName a) where traverseM _ = pure


---- ----- Unit -----

--instance Traversal base m a b => DefaultTraversal base m (Unit a) (Unit b) where
--    defaultTraverseM b (Unit a) = Unit <$> traverseM b a


---- ----- Module -----

--instance Traversal base m (LDecl f e) (LDecl f e) => DefaultTraversal base m (Module f e) (Module f e) where
--    defaultTraverseM b (Module path name body) = Module <$> traverseM b path <*> traverseM b name <*> traverseM b body


---- ----- Decl -----

--instance ( Traversal base m (LDecl a e)          (LDecl a f)
--         , Traversal base m (LCons a e)          (LCons a f)
--         , Traversal base m (Arg  a e)           (Arg  a f)
--         , Traversal base m (LType a)            (LType a)
--         , Traversal base m (Native (LDecl a e)) (Native (LDecl a f))
--         , Traversal base m ImpTgt               ImpTgt
--         , Traversal base m e f
--         ) => DefaultTraversal base m (Decl a e) (Decl a f) where
--    defaultTraverseM b = \case
--        Decl.Data        name params cons defs        -> Decl.Data        <$> traverseM b name <*> traverseM b params <*> traverseM b cons    <*> traverseM b defs
--        Decl.Function    path name inputs output body -> Decl.Function    <$> traverseM b path <*> traverseM b name   <*> traverseM b inputs  <*> traverseM b output <*> traverseM b body
--        Decl.Import      path rename targets          -> Decl.Import      <$> traverseM b path <*> traverseM b rename <*> traverseM b targets
--        Decl.TypeAlias   dst src                      -> Decl.TypeAlias   <$> traverseM b dst  <*> traverseM b src
--        Decl.TypeWrapper dst src                      -> Decl.TypeWrapper <$> traverseM b dst  <*> traverseM b src
--        Decl.Native      nat                          -> Decl.Native      <$> traverseM b nat

--instance Traversal base m (Decl.LField a e) (Decl.LField a e) => DefaultTraversal base m (Decl.Cons a e) (Decl.Cons a e) where
--    defaultTraverseM b (Decl.Cons name fields) = Decl.Cons <$> traverseM b name <*> traverseM b fields

--instance ( Traversal base m (LType a) (LType a)
--         , Traversal base m e f)
--         => DefaultTraversal base m (Decl.Field a e) (Decl.Field a f) where
--    defaultTraverseM b (Decl.Field tp name val) = Decl.Field <$> traverseM b tp <*> traverseM b name <*> traverseM b val


--instance DefaultTraversal base m Decl.ImpTgt Decl.ImpTgt where
--    defaultTraverseM _ = pure


---- ----- Type -----

--instance (Traversal base m (LType a) (LType a)) => DefaultTraversal base m (Type a) (Type a) where
--    defaultTraverseM b = \case
--        Type.Function inputs output -> Type.Function <$> traverseM b inputs <*> traverseM b output
--        Type.App      src args      -> Type.App      <$> traverseM b src    <*> traverseM b args
--        Type.Var      name          -> Type.Var      <$> traverseM b name
--        Type.Tuple    items         -> Type.Tuple    <$> traverseM b items
--        Type.List     item          -> Type.List     <$> traverseM b item
--        Type.Con      segments      -> Type.Con      <$> traverseM b segments
--        Type.Wildcard               -> pure Type.Wildcard


---- ----- Arg -----

--instance ( Traversal base m (LPat a) (LPat a)
--         , Traversal base m v g)
--         => DefaultTraversal base m (Arg a v) (Arg a g) where
--    defaultTraverseM b (Arg pat value) = Arg <$> traverseM b pat <*> traverseM b value



---- ----- Native -----

--instance DefaultTraversal base m (Native e) (Native e)


---- ----- Pat -----

--instance ( Traversal base m (LPat a)  (LPat a)
--         , Traversal base m (LLit a)  (LLit a)
--         , Traversal base m (LType a) (LType a)
--         ) => DefaultTraversal base m (Pat a) (Pat a) where
--    defaultTraverseM b = \case
--        Pat.App         src   args -> Pat.App         <$> traverseM b src   <*> traverseM b args
--        Pat.Typed       pat   cls  -> Pat.Typed       <$> traverseM b pat   <*> traverseM b cls
--        Pat.Grouped     pat        -> Pat.Grouped     <$> traverseM b pat
--        Pat.Lit         lit        -> Pat.Lit         <$> traverseM b lit
--        Pat.Tuple       items      -> Pat.Tuple       <$> traverseM b items
--        Pat.Con         name       -> Pat.Con         <$> traverseM b name
--        Pat.Var         name       -> Pat.Var         <$> traverseM b name
--        Pat.Wildcard               -> pure Pat.Wildcard
--        Pat.RecWildcard            -> pure Pat.RecWildcard



------ ----- Lit -----

--instance DefaultTraversal base m Lit Lit where defaultTraverseM _ = pure
