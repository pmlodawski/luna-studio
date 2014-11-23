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

import           Flowbox.Prelude
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
import           Luna.Pass2.Pass              (PassMonad, PassCtx)
import qualified Luna.Pass2.Pass              as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.AliasInfo          (AliasInfo)

import qualified Luna.Data.Namespace.State    as State 
import           Luna.Data.Namespace.State    (regVarName, regTypeName, withNewScope)
import qualified Luna.Parser.Parser           as Parser
import qualified Luna.Parser.State            as ParserState

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data AliasAnalysis = AliasAnalysis
data ParseStage2 = ParseStage2

type VAError           = String
type Stage2Pass m      = PassMonad VAError Namespace m
type Stage2Ctx lab m   = (Enumerated lab, Monad m) 
type MonoTraversal m a = AST.MonoTraversal AliasAnalysis (Stage2Pass m) a

--type Stage2Traversal m = 
--type Stage2Ctx lab m a = (PassCtx m, Enumerated lab, MonoTraversal m a)

type ResultExpr = LExpr IDTag (MultiName String)


------------------------------------------------------------------------
---- Utils functions
------------------------------------------------------------------------

traverseM :: (AST.Traversal ParseStage2 m a b, PassCtx m) => a -> m b
traverseM = AST.traverseM ParseStage2

--defaultTraverseM :: (AST.DefaultTraversal ParseStage2 m a a, PassCtx m) => a -> m a
--defaultTraverseM = AST.defaultMonoTraverseM ParseStage2


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------


--run :: Enumerated lab => Unit (LModule lab String) -> Pass.Result m VAError (Unit (LModule lab ResultExpr))
run :: (Enumerated a) =>
     Namespace -> Unit (LModule a String) -> Pass.Result (IO) VAError (Unit (LModule a ResultExpr))

run ns = (Pass.run_ (Pass.Info "Alias") mempty) . (aaUnit ns)


--aaUnit :: PassCtx m => Unit (LModule lab a) -> Stage2Pass m AliasInfo
aaUnit :: (Enumerated a, PassCtx m) =>
     Namespace -> Unit (LModule a String) -> Stage2Pass m (Unit (LModule a ResultExpr))
aaUnit ns (ast :: Unit (LModule a String)) = put ns *> traverseM ast -- *> (view Namespace.info <$> get)


traverseDecl2Pass :: Stage2Ctx lab m
                  => LDecl lab String -> Stage2Pass m (LDecl lab ResultExpr)

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
              result <- State.withScope id $ do 
                  ns <- get
                  let pstate = Parser.defState & set ParserState.namespace ns 
                  return $ Parser.parseString expr $ Parser.exprBlockParser2 pstate
              --result <- State.withScope id (Parser.parseString expr <$> (Parser.exprBlockParser2 <$> get))
              case result of
                  Left e      -> fail   $ show e
                  Right (e,_) -> return $ e
          -- FIXME [wd]: inny parser powinine parsowac argumenty poniewaz nie zawieraja wielu linii i nie moga zawierac wielu exproessionow!
          --             zatem wyciaganie pierwszego elementu jest szybkim obejsciem
          subparseArg (Arg pat val) = Arg pat . (fmap (!!0)) <$> mapM subparse val 


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Stage2Ctx lab m => AST.Traversal ParseStage2 (Stage2Pass m) (LDecl lab String) (LDecl lab ResultExpr) where
    traverseM _ = traverseDecl2Pass

