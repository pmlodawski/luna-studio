{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inference where


--import qualified Luna.Parser.Parser as Parser

import qualified Luna.ASTNew.Decl       as Decl
--import qualified Luna.ASTNew.Enum       as Enum
import qualified Luna.ASTNew.Label      as Label
--import qualified Luna.ASTNew.Module     as Module
--import qualified Luna.ASTNew.Name       as Name
--import qualified Luna.ASTNew.Unit       as Unit



--import           Luna.Data.Namespace               (Namespace (Namespace))
--import           Luna.Data.Source                  (Code (Code), Medium (String), Source (Source))
--import qualified Luna.Pass                         as Pass
--import qualified Luna.Pass2.Analysis.Struct        as AA
--import qualified Luna.Pass2.Transform.Parse.Stage1 as Stage1
--import qualified Luna.Pass2.Transform.Parse.Stage2 as Stage2
--import           Text.Show.Pretty                  (ppShow)

import qualified Luna.ASTNew.Traversals       as AST
import           Luna.ASTNew.Decl             (LDecl)
--import qualified Luna.Parser.State            as ParserState
import           Luna.Pass                    (PassMonad, PassCtx, Pass(Pass))
import           Luna.ASTNew.Enum             (Enumerated)
--import           Luna.ASTNew.Expr             (LExpr, Expr)
import           Luna.ASTNew.Module           (LModule)
import           Data.Monoid                  (Monoid, mempty)
import           Control.Monad.State          (get, modify)
--import           Luna.ASTNew.NameBase         (nameBase)
--import qualified Luna.ASTNew.Pat              as Pat
--import qualified Luna.ASTNew.Arg              as Arg
--import Data.Text.Lazy (unpack)
--import qualified Luna.Pass2.Analysis.Struct as SA
--import qualified Luna.Pass2.Transform.Hash                 as Hash
--import qualified Luna.Pass2.Target.HS.HASTGen              as HASTGen
--import qualified Luna.Pass2.Target.HS.HSC                  as HSC
--import qualified Luna.Pass2.Transform.SSA                  as SSA
--import qualified Luna.Pass2.Transform.Desugar.ImplicitSelf as ImplSelf

import Control.Applicative
--import Control.Monad
--import Control.Monad.Trans.Either

--import Control.Lens hiding (without)
--import Data.List    (intercalate,intersperse)




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

instance (StageTypecheckerCtx lab m a) => AST.Traversal StageTypechecker (StageTypecheckerPass m) (LModule lab a) (LModule lab a) where
  traverseM _ = tcMod

instance (StageTypecheckerCtx lab m a) => AST.Traversal StageTypechecker (StageTypecheckerPass m) (LDecl lab a) (LDecl lab a) where
  traverseM _ = tcDecl


tcDecl :: (StageTypecheckerCtx lab m a) => LDecl lab a -> StageTypecheckerPass m (LDecl lab a)
tcDecl ldecl@(Label.Label lab decl) = do
    case decl of
      fun@Decl.Data{}                         -> modify ("Data"                         :)
      fun@Decl.Function{} -> modify ("Function" :)
      -- fun@Decl.Function{ Decl._fname = fname, Decl._inputs = inputs }
                                              -- -> do let args = map mapArg inputs
                                              --      modify (("Function " ++ nameBase fname ++ " " ++ intercalate " " args):)
      fun@Decl.Import{}                       -> modify ("Import"                       :)
      fun@Decl.TypeAlias{}                    -> modify ("TypeAlias"                    :)
      fun@Decl.TypeWrapper{}                  -> modify ("TypeWrapper"                  :)
      fun@Decl.Native{}                       -> modify ("Native"                       :)
    defaultTraverseM ldecl
  --where
  --  --mapArg :: Arg.Arg lab a -> String
  --  mapArg Arg.Arg{Arg._pat = (Label.Label _ (Pat.Var {Pat._vname = (Name.VName vname)}))}  = show vname
  --  mapArg _                                                                                = "?"

tcMod :: (StageTypecheckerCtx lab m a) => LModule lab a -> StageTypecheckerPass m (LModule lab a)
tcMod modulearg = modify ("456":) *> defaultTraverseM modulearg

tcUnit :: (StageTypecheckerDefaultTraversal m a) => a -> StageTypecheckerPass m StageTypecheckerState
tcUnit ast = do
  modify ("123123":)
  defaultTraverseM ast *> get