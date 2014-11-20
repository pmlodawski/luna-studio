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

module Luna.Pass2.Analysis.Alias.Alias where

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
import qualified Luna.ASTNew.Lit    as Lit
import           Luna.ASTNew.Arg    (Arg(Arg))
import qualified Luna.ASTNew.Native as Native
import           Luna.ASTNew.Name.Multi       (MultiName(MultiName))
import qualified Luna.ASTNew.Name.Multi       as MultiName
import qualified Luna.ASTNew.Name             as Name
import           Luna.ASTNew.Name             (TName(TName), TVName(TVName))
import qualified Luna.Parser.State            as State
import           Luna.Parser.State            (State)
import           Luna.Pass2.Pass              (Pass)
import qualified Luna.Pass2.Pass              as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.AliasInfo          (AliasInfo)

import qualified Luna.Data.Namespace.State    as State2 

--logger :: LoggerIO
--logger = getLoggerIO $(moduleName)


--type VAPass a e v result = Pass (Namespace a e v) result


--type PassT state result m = ESRT PassError Info state m result

--type Pass state result = (Functor m, MonadIO m) => ESRT PassError Info state m result

--type Result result = (Functor m, MonadIO m) => m (Either PassError result)

--type ResultT m = EitherT PassError m

--type ESRT err env state m = EitherT err (StateT state (ReaderT env m))


type PassMonad err state m = Pass.ESRT err Pass.Info state m

type AACtx2 a e v m = (Enumerated a, AST.Traversal AliasAnalysis (PassMonad Pass.PassError (Namespace a e v) m) e e)

run :: (Enumerated a, AST.Traversal AliasAnalysis (PassMonad Pass.PassError (Namespace a e v) m) e e) =>
     Unit (LModule a e) -> Pass.Result m (AliasInfo a e v)
run = (Pass.run_ (Pass.Info "Alias") mempty) . vaMod2


--vaMod :: Unit (LModule a e) -> VAPass a e v (AliasInfo a e v)
--vaMod = vaMod2

--vaMod2 :: Unit (LModule a e) -> VAPass a e v (Unit (LModule a e))
--vaMod2 :: (AST.Traversal AliasAnalysis m a b, Applicative m, Monad m) => a -> m b
--vaMod2 :: (AST.Traversal AliasAnalysis m e e, Applicative m, Monad m) =>
--          Unit (LModule a e) -> m (Unit (LModule a e))

vaMod2 (ast :: Unit (LModule a e)) = traverseM ast *> (view Namespace.info <$> get)

data AliasAnalysis = AliasAnalysis

--traverseM :: Applicative m => a -> m b
traverseM        = AST.traverseM        AliasAnalysis
defaultTraverseM = AST.defaultTraverseM AliasAnalysis

testme ast st = runState (traverseM ast) st


type AACtx m lab e s v = (Enumerated lab, MonadState (Namespace s e v) m, Applicative m)

instance (AACtx m lab e s v, AST.Traversal AliasAnalysis m a a)
    => AST.Traversal AliasAnalysis m (LModule lab a) (LModule lab a) where
    traverseM _ = aatest

instance (AACtx m lab e s v, AST.Traversal AliasAnalysis m a a)
      => AST.Traversal AliasAnalysis m (LDecl lab a) (LDecl lab a) where
    traverseM _ = traverseDecl

instance AACtx m lab e s v
      => AST.Traversal AliasAnalysis m (LPat lab) (LPat lab) where
    traverseM _ = registerPat


aaunit (Unit mod) = Unit <$> aatest mod

aatest mod@(Label lab (Module path name body)) = State2.withNewScope id continue
        where continue =  registerDecls body
                       *> defaultTraverseM mod
              id       = Enum.id lab

--registerDecls :: (Enumerated a, AST.Traversal AliasAnalysis m t v',
--                 MonadState (Namespace s e v) m, Applicative m) =>
--                 [LDecl a t] -> m ()
registerDecls decls =  mapM_ registerHeaders  decls
                    *> mapM_ registerDataDecl decls


registerDataDecl (Label lab decl) = case decl of
    Decl.Data     name _ cons defs   -> State2.withNewScope id (registerDecls defs) *> pure ()
    _                                -> pure ()
    where id = Enum.id lab

registerHeaders (Label lab decl) = case decl of
    Decl.Function _ name inputs _ _  -> State2.regVarName id (view MultiName.base name)
                                     <* State2.withNewScope id (traverseM inputs)
    Decl.Data     name _ cons _      -> State2.regTypeName id (Name.fromName name) 
                                     <* mapM_ registerCons cons
    _                                -> pure ()
    where id = Enum.id lab

--registerPat p@(Label lab pat) = case pat of
--    Pat.Var         name       -> State.regVarName id (Name.fromName name) *> continue
--    _                          -> continue
--    where id = Enum.id lab
--          continue = defaultTraverseM p 

registerPat p@(Label lab pat) = case pat of
    Pat.Var         name       -> State2.regVarName id (Name.fromName name) *> continue
    _                          -> continue
    where id = Enum.id lab
          continue = defaultTraverseM p 

registerCons (Label lab (Decl.Cons name fields)) = State2.regVarName (Enum.id lab) (Name.fromName name)


traverseDecl d@(Label lab decl) = case decl of
    Decl.Function path name inputs output body -> State2.withNewScope id $ defaultTraverseM d
    _                                          -> continue
    where id       = Enum.id lab
          continue = defaultTraverseM d