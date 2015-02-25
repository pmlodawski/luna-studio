{-# LANGUAGE ConstraintKinds #-}

module Luna.Typechecker.Inference.Class where


import Flowbox.Prelude                              hiding (Traversal)

import Luna.Pass                                    (PassCtx, PassMonad)

import Luna.Syntax.Decl                             (LDecl)
import Luna.Syntax.Enum                             (Enumerated, IDTag)
import Luna.Syntax.Expr                             (LExpr)
import Luna.Syntax.Pat                              (Pat(Pat))
import Luna.Syntax.Traversals                       (DefaultTraversal, Traversal)

import Luna.System.Session                          (SessionMonad)

import Luna.Typechecker.Debug.HumanName             (HumanName)
import Luna.Typechecker.StageTypecheckerState.Class (StageTypecheckerState)



data StageTypechecker = StageTypechecker

type StageTypecheckerPass                 m   = PassMonad StageTypecheckerState m
type StageTypecheckerCtx              lab m   = (HumanName (Pat lab), Enumerated lab, Monad m, Applicative m, SessionMonad m, PassCtx m)
type StageTypecheckerTraversal            m a = (PassCtx m, Traversal        StageTypechecker (StageTypecheckerPass m) a a)
type StageTypecheckerDefaultTraversal     m a = (PassCtx m, DefaultTraversal StageTypechecker (StageTypecheckerPass m) a a)

type InExpr  = (LExpr IDTag ())
type OutExpr = (LExpr IDTag ()) 

type InDecl  = (LDecl IDTag InExpr)
type OutDecl = (LDecl IDTag InExpr)
