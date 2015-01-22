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

module Luna.Pass.Transform.Parse.Stage2 where

import           Flowbox.Prelude              hiding (Traversal)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Luna.Syntax.Traversals       as AST
import qualified Luna.Syntax.Enum       as Enum
import           Luna.Syntax.Enum       (Enumerated, IDTag(IDTag))
import qualified Luna.Syntax.Decl   as Decl
import           Luna.Syntax.Decl   (LDecl, Field(Field), LField)
import qualified Luna.Syntax.Module as Module
import           Luna.Syntax.Module (Module(Module), LModule)
import           Luna.Syntax.Unit   (Unit(Unit))
import qualified Luna.Syntax.Label  as Label
import           Luna.Syntax.Label  (Label(Label))
import qualified Luna.Syntax.Type   as Type
import           Luna.Syntax.Type   (Type)
import qualified Luna.Syntax.Pat    as Pat
import           Luna.Syntax.Pat    (LPat, Pat)
import           Luna.Syntax.Expr   (LExpr, Expr)
import qualified Luna.Syntax.Lit    as Lit
import           Luna.Syntax.Arg    (Arg(Arg))
--import           Luna.Syntax.Arg    (Arg(Arg))
import qualified Luna.Syntax.Native as Native
import           Luna.Syntax.Name.Path        (NamePath(NamePath))
import qualified Luna.Syntax.Name.Path        as NamePath
import qualified Luna.Syntax.Name             as Name
import           Luna.Syntax.Name             (TName(TName), TVName(TVName))
import           Luna.Pass              (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass              as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.ASTInfo            (ASTInfo)

import qualified Luna.Data.Namespace.State    as State 
import qualified Luna.Parser.Parser           as Parser
import qualified Luna.Parser.State            as ParserState
import           Luna.Parser.State            (ParserState)

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data Stage2 = Stage2

type Stage2Pass             m     = PassMonad ParserState m
type Stage2Ctx              lab m = (Enumerated lab, PassCtx m)
type Stage2Traversal        m a b = (PassCtx m, AST.Traversal        Stage2 (Stage2Pass m) a b)
type Stage2DefaultTraversal m a b = (PassCtx m, AST.DefaultTraversal Stage2 (Stage2Pass m) a b)

type ResultExpr = LExpr IDTag ()


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

pass :: Stage2DefaultTraversal m a b => Pass ParserState (Namespace -> ASTInfo -> a -> Stage2Pass m (b,ASTInfo))
pass = Pass "Parser stage-2" "Parses expressions based on AST stage-1 and structural analysis" undefined passRunner

-- FIXME[wd]: using emptyState just to make it working
--            we should use here state constructed from config optained from stage1
--            but stage-1 should NOT result in whole ParserState - the data should be separated
passRunner ns info ast = do
    put (Parser.emptyState & set ParserState.namespace ns & set ParserState.info info)
    (,) <$> defaultTraverseM ast <*> (view ParserState.info <$> get)


traverseDecl :: Stage2Ctx lab m => LDecl lab String -> Stage2Pass m (LDecl lab ResultExpr)
traverseDecl e@(Label lab decl) = fmap (Label lab) $ case decl of
    Decl.Func (Decl.FuncDecl path sig output body) 
                                     -> do subAST <- subparse (unlines body)
                                           sig'   <- mapM (subparseArg id) sig
                                           return . Decl.Func $ Decl.FuncDecl path sig' output subAST
    Decl.Data (Decl.DataDecl name params cons defs)
                                     -> Decl.Data <$> (Decl.DataDecl name params <$> defaultTraverseM cons 
                                                                                 <*> defaultTraverseM defs
                                                      )
    Decl.Imp   path rename targets   -> return $ Decl.Imp   path rename targets
    Decl.TpAls dst src               -> return $ Decl.TpAls dst src
    Decl.TpWrp dst src               -> return $ Decl.TpWrp dst src
    Decl.Foreign fdecl               -> Decl.Foreign <$> mapM (traverseFDecl id) fdecl
    Decl.Pragma p                    -> return $ Decl.Pragma p
    where id       = Enum.id lab
          --continue = defaultTraverseM e 
          subparse expr = do
              result <- ParserState.withScope id $ do 
                  pstate <- get
                  Parser.parseString expr $ Parser.exprBlockParser2 pstate
              case result of
                  Left e      -> fail   $ show e
                  Right (e,s) -> put s *> pure e

-- FIXME [wd]: just clean and make nicer code
subparseArg id (Arg pat mexpr) = Arg pat <$> mapM (subparseInlineExpr id) mexpr

subparseInlineExpr id expr = do
              result <- ParserState.withScope id $ do 
                  pstate <- get
                  Parser.parseString expr $ Parser.exprParser2 pstate
              case result of
                  Left e      -> fail   $ show e
                  Right (e,s) -> put s *> pure e


--traverseDecl :: Stage2Ctx lab m => LDecl lab String -> Stage2Pass m (LDecl lab ResultExpr)
traverseFDecl id = \case
    Decl.FFunc (Decl.FuncDecl path sig output body) 
                                     -> do sig'   <- mapM (subparseArg id) sig
                                           return . Decl.FFunc $ Decl.FuncDecl path sig' output body
    Decl.FData (Decl.DataDecl name params cons defs)
                                     -> Decl.FData <$> (Decl.DataDecl name params <$> defaultTraverseM cons 
                                                                                 <*> defaultTraverseM defs
                                                      )


traverseField :: Stage2Ctx lab m => LField lab String -> Stage2Pass m (LField lab ResultExpr)
traverseField (Label lab (Field tp name val)) = (Label lab . Field tp name) <$> mapM (subparseInlineExpr id) val
    where id = Enum.id lab

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Stage2Ctx lab m => AST.Traversal Stage2 (Stage2Pass m) (LDecl lab String) (LDecl lab ResultExpr) where
    traverseM _ = traverseDecl

instance Stage2Ctx lab m => AST.Traversal Stage2 (Stage2Pass m) (LField lab String) (LField lab ResultExpr) where
    traverseM _ = traverseField

