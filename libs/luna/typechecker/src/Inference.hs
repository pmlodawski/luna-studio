{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- *------------------------------------------------
-- * Specification of the generic HM(X)
-- * type inference system in Haskell
-- *
-- * This instance deals with Ohori style records
-- *------------------------------------------------


module Inference where


import            Luna.Pass                 (PassMonad, PassCtx, Pass(Pass))
import qualified  Luna.ASTNew.Decl          as Decl
import            Luna.ASTNew.Decl          (LDecl)
import            Luna.ASTNew.Enum          (Enumerated)
import qualified  Luna.ASTNew.Label         as Label
import qualified  Luna.ASTNew.Module        as Module
import            Luna.ASTNew.Module        (LModule)
import qualified  Luna.ASTNew.Name.Pattern2 as Pat2
import            Luna.ASTNew.NameBase      (NameBase(nameBase))
import qualified  Luna.ASTNew.Pat           as Pat
import qualified  Luna.ASTNew.Traversals    as AST

import            Control.Monad.State       (get, modify)
import            Data.List                 (intercalate)
import            Data.Monoid               (Monoid, mempty)


data StageTypechecker = StageTypechecker

type StageTypecheckerState = [String]

type StageTypecheckerPass             m       = PassMonad StageTypecheckerState m
type StageTypecheckerCtx              lab m a = (Enumerated lab, StageTypecheckerTraversal m a)
type StageTypecheckerTraversal        m   a   = (PassCtx m, AST.Traversal        StageTypechecker (StageTypecheckerPass m) a a)
type StageTypecheckerDefaultTraversal m   a   = (PassCtx m, AST.DefaultTraversal StageTypechecker (StageTypecheckerPass m) a a)


traverseM :: (StageTypecheckerTraversal m a) => a -> StageTypecheckerPass m a
traverseM = AST.traverseM StageTypechecker

defaultTraverseM :: (StageTypecheckerDefaultTraversal m a) => a -> StageTypecheckerPass m a
defaultTraverseM = AST.defaultTraverseM StageTypechecker


tcpass :: (Monoid s, StageTypecheckerDefaultTraversal m a) => Pass s (a -> StageTypecheckerPass m StageTypecheckerState)
tcpass = Pass "Typechecker"
              "Performs typechecking"
              mempty
              tcUnit

instance (StageTypecheckerCtx lab m a, NameBase (Pat.Pat lab)) => AST.Traversal StageTypechecker (StageTypecheckerPass m) (LModule lab a) (LModule lab a) where
  traverseM _ = tcMod

instance (StageTypecheckerCtx lab m a, NameBase (Pat.Pat lab)) => AST.Traversal StageTypechecker (StageTypecheckerPass m) (LDecl lab a) (LDecl lab a) where
  traverseM _ = tcDecl


tcDecl :: (NameBase (Pat.Pat lab), StageTypecheckerCtx lab m a) => LDecl lab a -> StageTypecheckerPass m (LDecl lab a)
tcDecl ldecl@(Label.Label lab decl) = do
    case decl of
      --fun@Decl.Function{} -> modify ("Function" :)
      fun@Decl.Function{ Decl._sig = sig@Pat2.NamePat{ Pat2._base = (Pat2.Segment name args) } }
                                              -> do let argsS = map mapArg args
                                                    modify (("Function " ++ name ++ " " ++ intercalate " " argsS) :)
      fun@Decl.Data{}                         -> modify ("Data"                         :)
      fun@Decl.Import{}                       -> modify ("Import"                       :)
      fun@Decl.TypeAlias{}                    -> modify ("TypeAlias"                    :)
      fun@Decl.TypeWrapper{}                  -> modify ("TypeWrapper"                  :)
      fun@Decl.Native{}                       -> modify ("Native"                       :)
    defaultTraverseM ldecl
  where
    mapArg :: (NameBase (Pat.Pat lab)) => Pat2.Arg (Pat.LPat lab) a -> String
    mapArg (Pat2.Arg (Label.Label _ arg) _) = nameBase arg
  --  --mapArg :: Arg.Arg lab a -> String
  --  mapArg Arg.Arg{Arg._pat = (Label.Label _ (Pat.Var {Pat._vname = (Name.VName vname)}))}  = show vname
  --  mapArg _                                                                                = "?"

tcMod :: (StageTypecheckerCtx lab m a, NameBase (Pat.Pat lab)) => LModule lab a -> StageTypecheckerPass m (LModule lab a)
tcMod lmodule@(Label.Label _ Module.Module {Module._path = path, Module._name = name, Module._body = body} ) = do
  modify (("Module " ++ intercalate "." (path ++ [name])):)
  defaultTraverseM lmodule

tcUnit :: (StageTypecheckerDefaultTraversal m a) => a -> StageTypecheckerPass m StageTypecheckerState
tcUnit ast = do
  modify ("First!" :)
  _ <- defaultTraverseM ast
  get



---- type inference

--tp :: (Typo, Term) -> TP Type
--tp (env, Id x) =  do a <- inst env x
--                     normalize a
----
--tp (env, Abs x e) = do a <- newtvar
--                       b <- tp (insert env (x, Mono (TV a)), e)
--                       normalize ((TV a) `Fun` b)

--tp (env, App e e') = do a <- newtvar
--                        t <- tp (env, e)
--                        t' <- tp (env, e')
--                        add_constraint (C [t `Subsume` (t' `Fun` TV a)])
--                        normalize (TV a)


--tp (env, Let x e e') = do a <- tp (env, e)
--                          b <- gen env a
--                          tp ((insert env (x, b)), e')

---- top-level program

--infer :: Term -> E (TVar, Subst, Constraint, Type)
--infer e = unTP (tp (init_typo, e)) (init_tvar, null_subst, true_cons)
----


