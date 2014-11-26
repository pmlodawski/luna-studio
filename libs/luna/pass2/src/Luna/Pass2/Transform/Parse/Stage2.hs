---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

module Luna.Pass2.Transform.Parse.Stage2 where

import           Flowbox.Prelude              hiding (Traversal)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Luna.ASTNew.Traversals       as AST
import qualified Luna.ASTNew.Enum       as Enum
import           Luna.ASTNew.Enum       (Enumerated, IDTag(IDTag))
import qualified Luna.ASTNew.Decl   as Decl
import           Luna.ASTNew.Decl   (LDecl, Field(Field))
import qualified Luna.ASTNew.Module as Module
import           Luna.ASTNew.Module (Module(Module), LModule)
import           Luna.ASTNew.Unit   (Unit(Unit))
import qualified Luna.ASTNew.Label  as Label
import           Luna.ASTNew.Label  (Label(Label))
import qualified Luna.ASTNew.Type   as Type
import           Luna.ASTNew.Type   (Type)
import qualified Luna.ASTNew.Pat    as Pat
import           Luna.ASTNew.Pat    (LPat, Pat)
import           Luna.ASTNew.Expr   (LExpr, Expr)
import qualified Luna.ASTNew.Lit    as Lit
import           Luna.ASTNew.Arg    (Arg(Arg))
import qualified Luna.ASTNew.Native as Native
import           Luna.ASTNew.Name.Multi       (MultiName(MultiName))
import qualified Luna.ASTNew.Name.Multi       as MultiName
import qualified Luna.ASTNew.Name             as Name
import           Luna.ASTNew.Name             (TName(TName), TVName(TVName))
import           Luna.Pass              (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass              as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.AliasInfo          (AliasInfo)
import           Luna.Data.ASTInfo            (ASTInfo)

import qualified Luna.Data.Namespace.State    as State 
import           Luna.Data.Namespace.State    (regVarName, regTypeName, withNewScope)
import qualified Luna.Parser.Parser           as Parser
import qualified Luna.Parser.State            as ParserState

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data Stage2 = Stage2

type Stage2Pass             m     = PassMonad (ParserState.State ()) m
type Stage2Ctx              lab m = (Enumerated lab, PassCtx m)
type Stage2Traversal        m a b = (PassCtx m, AST.Traversal        Stage2 (Stage2Pass m) a b)
type Stage2DefaultTraversal m a b = (PassCtx m, AST.DefaultTraversal Stage2 (Stage2Pass m) a b)

type ResultExpr = LExpr IDTag (MultiName String)


------------------------------------------------------------------------
---- Utils functions
------------------------------------------------------------------------

traverseM :: (Stage2Traversal m a b) => a -> Stage2Pass m b
traverseM = AST.traverseM Stage2

defaultTraverseM :: (Stage2DefaultTraversal m a b) => a -> Stage2Pass m b
defaultTraverseM = AST.defaultTraverseM Stage2


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------

pass :: Stage2DefaultTraversal m a b => Pass (ParserState.State ()) (Namespace -> ASTInfo -> a -> Stage2Pass m b)
pass = Pass "Parser stage-2" "Parses expressions based on AST stage-1 and alias analysis" undefined passRunner

-- FIXME[wd]: using emptyState just to make it working
--            we should use here state constructed from config optained from stage1
--            but stage-1 should NOT result in whole ParserState - the data should be separated
passRunner ns info ast = put (Parser.emptyState & set ParserState.namespace ns & set ParserState.info info) *> defaultTraverseM ast 

traverseDecl2Pass :: Stage2Ctx lab m => LDecl lab String -> Stage2Pass m (LDecl lab ResultExpr)
traverseDecl2Pass (Label lab decl) = fmap (Label lab) $ case decl of
    Decl.Function path name inputs output body -> do
        subAST  <- subparse (unlines body)
        inputs' <- mapM subparseArg inputs
        return $ Decl.Function path name inputs' output subAST
    Decl.Data        name params cons defs -> return $ Decl.Data        name params [] []
    Decl.Import      path rename targets   -> return $ Decl.Import      path rename targets
    Decl.TypeAlias   dst src               -> return $ Decl.TypeAlias   dst src
    Decl.TypeWrapper dst src               -> return $ Decl.TypeWrapper dst src
    where id = Enum.id lab
          subparse expr = do
              result <- ParserState.withScope id $ do 
                  pstate <- get
                  return $ Parser.parseString expr $ Parser.exprBlockParser2 (pstate)
              case result of
                  Left e      -> fail   $ show e
                  Right (e,s) -> put s *> pure e
          -- FIXME [wd]: inny parser powinine parsowac argumenty poniewaz nie zawieraja wielu linii i nie moga zawierac wielu exproessionow!
          --             zatem wyciaganie pierwszego elementu jest szybkim obejsciem
          subparseArg (Arg pat val) = Arg pat . (fmap (!!0)) <$> mapM subparse val 


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Stage2Ctx lab m => AST.Traversal Stage2 (Stage2Pass m) (LDecl lab String) (LDecl lab ResultExpr) where
    traverseM _ = traverseDecl2Pass

