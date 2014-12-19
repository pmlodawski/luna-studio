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
import qualified  Luna.ASTNew.Expr          as Expr
import            Luna.ASTNew.Expr          (LExpr)
import qualified  Luna.ASTNew.Label         as Label
import            Luna.ASTNew.Label         (Label(Label))
import qualified  Luna.ASTNew.Module        as Module
import            Luna.ASTNew.Module        (LModule)
import qualified  Luna.ASTNew.Name.Pattern  as NamePat
import qualified  Luna.ASTNew.Pat           as Pat
import qualified  Luna.ASTNew.Traversals    as AST

import            Control.Applicative       ((<$>))
import            Control.Monad.State       (get, modify)
import            Data.List                 (intercalate)
import            Data.Monoid               (Monoid, mempty)
import            Data.Text.Lazy            (unpack)

import            HumanName                 (HumanName(humanName))


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


tcpass :: (Monoid s, StageTypecheckerDefaultTraversal m a) => Pass s (a -> t -> StageTypecheckerPass m StageTypecheckerState)
tcpass = Pass "Typechecker"
              "Performs typechecking"
              mempty
              tcUnit

instance (StageTypecheckerCtx lab m a, HumanName (Pat.Pat lab)) => AST.Traversal StageTypechecker (StageTypecheckerPass m) (LModule lab a)  (LModule lab a) where traverseM _ = tcMod
instance (StageTypecheckerCtx lab m a, HumanName (Pat.Pat lab)) => AST.Traversal StageTypechecker (StageTypecheckerPass m) (LDecl lab a)    (LDecl lab a)   where traverseM _ = tcDecl
instance (StageTypecheckerCtx lab m a, HumanName (Pat.Pat lab)) => AST.Traversal StageTypechecker (StageTypecheckerPass m) (LExpr lab a)    (LExpr lab a)   where traverseM _ = tcExpr


tcExpr :: (StageTypecheckerCtx lab m a) => LExpr lab a -> StageTypecheckerPass m (LExpr lab a)
tcExpr lexpr@(Label lab expr) = do
    case expr of 
      Expr.Var { Expr._ident = (Expr.Variable vname _) }
          -> do let hn = unpack . humanName $ vname
                modify (("Var         " ++ hn) :)
      Expr.Assignment { Expr._dst = (Label _ dst), Expr._src = (Label _ src) }
          -> do case (dst, src) of
                  (Pat.Var { Pat._vname = dst_vname }, Expr.Var { Expr._ident = (Expr.Variable src_vname _) }) ->
                      modify (("Assignment " ++ (unpack . humanName $ dst_vname) ++ " <- " ++ (unpack . humanName $ src_vname)) :)
                  _ -> modify ("Some assignment..." : )
      Expr.App (NamePat.NamePat { NamePat._base = (NamePat.Segment (Label _ (Expr.Var { Expr._ident = (Expr.Variable basename _)})) args)})
          -> modify (("Application " ++ (unpack . humanName $ basename) ++ " ( " ++ intercalate " " (map mapArg args) ++ " )") : )
      _   -> return ()
    defaultTraverseM lexpr
  where
    mapArg :: Expr.AppArg (LExpr lab a) -> String
    mapArg (Expr.AppArg _ (Label _ (Expr.Var { Expr._ident = (Expr.Variable vname _) } ))) = unpack . humanName $ vname
    -- mapArg (Label _ (Expr.Var { Expr._ident = (Expr.Variable vname _) })) = unpack . humanName $ vname

tcDecl :: (HumanName (Pat.Pat lab), StageTypecheckerCtx lab m a) => LDecl lab a -> StageTypecheckerPass m (LDecl lab a)
tcDecl ldecl@(Label lab decl) = do
    case decl of
      fun@Decl.Func { Decl._sig  = sig@NamePat.NamePat{ NamePat._base = (NamePat.Segment name args) }
                    , Decl._body = body
                    }
          -> do let argsS = fmap mapArg args
                modify (("Function    " ++ unpack name ++ " " ++ unwords argsS) :)
      _                  -> return ()
    defaultTraverseM ldecl
  where
    mapArg :: (HumanName (Pat.Pat lab)) => NamePat.Arg (Pat.LPat lab) a -> String
    mapArg (NamePat.Arg (Label _ arg) _) = unpack $ humanName arg

tcMod :: (StageTypecheckerCtx lab m a, HumanName (Pat.Pat lab)) => LModule lab a -> StageTypecheckerPass m (LModule lab a)
tcMod lmodule@(Label _ Module.Module {Module._path = path, Module._name = name, Module._body = body} ) = do
    modify (("Module      " ++ intercalate "." (fmap unpack (path ++ [name]))):)
    defaultTraverseM lmodule

tcUnit :: (StageTypecheckerDefaultTraversal m a) => a -> t -> StageTypecheckerPass m StageTypecheckerState
tcUnit ast _ = do
    modify ("First!" :)
    _ <- defaultTraverseM ast
    reverse <$> get



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



