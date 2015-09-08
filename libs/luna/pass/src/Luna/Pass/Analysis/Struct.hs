---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Luna.Pass.Analysis.Struct where

import           Flowbox.Prelude
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Luna.Syntax.Traversals       as AST
import qualified Luna.Syntax.Enum             as Enum
import           Luna.Syntax.Enum             (Enumerated, IDTag(IDTag))
import qualified Luna.Syntax.Decl             as Decl
import           Luna.Syntax.Decl             (LDecl, Field(Field))
import qualified Luna.Syntax.Module           as Module
import           Luna.Syntax.Module           (Module(Module), LModule)
import           Luna.Syntax.Unit             (Unit(Unit))
import qualified Luna.Syntax.Label            as Label
import           Luna.Syntax.Label            (Label(Label))
import qualified Luna.Syntax.Type             as Type
import           Luna.Syntax.Type             (Type)
import           Luna.Syntax.Expr             (LExpr, Expr)
import qualified Luna.Syntax.Expr             as Expr
import qualified Luna.Syntax.Pat              as Pat
import           Luna.Syntax.Pat              (LPat, Pat)
import qualified Luna.Syntax.Lit              as Lit
import           Luna.Syntax.Arg              (Arg(Arg))
import qualified Luna.Syntax.Native           as Native
import           Luna.Syntax.Name.Path        (NamePath(NamePath))
import qualified Luna.Syntax.Name.Path        as NamePath
import qualified Luna.Syntax.Name             as Name
import           Luna.Syntax.Name             (TName(TName), TVName(TVName), VName(VName))
import           Luna.Pass                    (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass                    as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.StructInfo         (StructInfo, OriginInfo(OriginInfo))
import qualified Luna.Data.StructInfo         as SI

import           Luna.Data.StructData         (StructData)
import qualified Luna.Data.StructData         as StructData

import qualified Luna.Data.ModuleInfo         as ModuleInfo

import qualified Luna.Data.Namespace.State    as State 
import           Luna.Data.Namespace.State    (regOrphan, regAlias, regParent, regVarName, regNamePatDesc, regTypeName, withScope)
import qualified Luna.Parser.State            as ParserState
import qualified Luna.Syntax.Name.Pattern     as NamePattern
import           Luna.Syntax.Foreign          (Foreign(Foreign))

import qualified Data.IntMap                  as IntMap
import qualified Luna.Data.ImportInfo         as II

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data StructAnalysis = StructAnalysis

type SAPass                 m   = PassMonad StructData m
type SACtx              lab m a = (Enumerated lab, SATraversal m a, MonadIO m)
type SATraversal            m a = (PassCtx m, AST.Traversal        StructAnalysis (SAPass m) a a)
type SADefaultTraversal     m a = (PassCtx m, AST.DefaultTraversal StructAnalysis (SAPass m) a a)

----------------------------------------------------------------------
-- Utils functions
----------------------------------------------------------------------

traverseM :: (SATraversal m a) => a -> SAPass m a
traverseM = AST.traverseM StructAnalysis

defaultTraverseM :: (SADefaultTraversal m a) => a -> SAPass m a
defaultTraverseM = AST.defaultTraverseM StructAnalysis

----------------------------------------------------------------------
-- Pass functions
----------------------------------------------------------------------

pass :: (Monoid s, SADefaultTraversal m a) => Pass s (StructData -> a -> SAPass m StructData)
pass = Pass "Alias analysis" 
            "Basic alias analysis that results in scope, alias, orphans and parent mapping information" 
            mempty aaUnit

aaUnit :: SADefaultTraversal m a => StructData -> a -> SAPass m StructData
aaUnit sd ast = put sd *> defaultTraverseM ast *> get

aaMod :: SACtx lab m a => LModule lab a -> SAPass m (LModule lab a)
aaMod mod@(Label lab (Module path body)) = withScope id continue
    where continue = StructData.setPath path
                   *> registerDecls body
                   *> defaultTraverseM mod
          id       = Enum.id lab

aaPat :: (PassCtx m, Enumerated lab) => LPat lab -> SAPass m (LPat lab)
aaPat p@(Label lab pat) = case pat of
    Pat.Var         name       -> regParent id
                               *> StructData.regVarNameLocal id (unwrap name)
                               *> continue
    _                          -> continue
    where id = Enum.id lab
          continue = defaultTraverseM p 

aaDecl :: SACtx lab m a => LDecl lab a -> SAPass m (LDecl lab a)
aaDecl d@(Label lab decl) = withScope id continue
    where id       = Enum.id lab
          continue = defaultTraverseM d

aaExpr :: SACtx lab m a => LExpr lab a -> SAPass m (LExpr lab a)
aaExpr e@(Label lab expr) = case expr of
    var@(Expr.Var (Expr.Variable vname@(VName name) _)) -> regParent id
                                         -- *> regAlias id (unwrap vname)
                                          *> StructData.regVarName id name
                                          *> continue
    cons@(Expr.Cons name)                 -> regParent id 
                                          *> StructData.regVarName id (unwrap name)  
                                         -- *> regAlias id (unwrap name) 
                                          *> continue
    Expr.RecUpd name _                    -> regParent  id
                                          *> regAlias   id (unwrap name)
                                          *> regVarName (OriginInfo "dupa" id) (unwrap name)
                                          *> continue
    _                                     -> continue
    where id       = Enum.id lab
          continue = defaultTraverseM e


registerDecls :: SACtx lab m a => [LDecl lab a] -> SAPass m ()
registerDecls decls =  mapM_ registerHeaders  decls
                    *> mapM_ registerDataDecl decls

registerDataDecl :: SACtx lab m a => LDecl lab a -> SAPass m ()
registerDataDecl (Label lab decl) = case decl of
    Decl.Data (Decl.DataDecl name _ cons defs) -> withScope id $ do
        mapM_ registerCons cons
        registerDecls defs
        pure ()
    _                                          -> pure ()
    where id = Enum.id lab
          registerCons (Label lab (Decl.Cons _ fields)) = mapM registerField fields
          registerField (Label lab (Decl.Field t mn v)) = case mn of
              Nothing -> return ()
              Just n  -> StructData.regVarNameLocal (Enum.id lab) (unwrap n)   -- TDOO[PMo] regTypeName, probably


registerHeaders :: (SACtx lab m a) => LDecl lab a -> SAPass m ()
registerHeaders (Label lab decl) = case decl of
    Decl.Func    fdecl -> regFuncDecl id fdecl
    Decl.Data    ddecl -> regDataDecl id ddecl
    Decl.Foreign fdecl -> regForeignDecl id fdecl
    --Decl.Imp     imp   -> regImport id imp
    _                  -> pure ()
    where id = Enum.id lab
          
regForeignDecl id (Foreign tgt fdecl) = case fdecl of
    Decl.FData ddecl -> regDataDecl id ddecl
    Decl.FFunc fdecl -> regFuncDecl id fdecl
    _                -> pure ()

--regImport id imp = do
--    let path = Decl._modPath imp
--    exists <- liftIO $ ModuleInfo.moduleExists path
--    unless exists $ regOrphan id (SI.ImportError path "Module not found.") -- TODO errors go to ModuleInfo now!
    



-- regParent here means that the class will be the parent of its methods. Do we need it? Seems sane. 
regFuncDecl id (Decl.FuncDecl _ sig _ _) = StructData.regVarNameLocal id (NamePattern.toNamePath sig)
                                         <* regNamePatDesc id (NamePattern.toDesc sig)
regDataDecl id (Decl.DataDecl name _ cons _) =  StructData.regTypeNameLocal id (unwrap name) 
                                             <* mapM_ registerCons cons
    where registerCons (Label lab (Decl.Cons name fields)) = StructData.regVarNameLocal (Enum.id lab) (unwrap name) -- TODO[PMo] reg typename

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance SACtx lab m a => AST.Traversal StructAnalysis (SAPass m) (LModule lab a) (LModule lab a) where
    traverseM _ = aaMod

instance SACtx lab m a => AST.Traversal StructAnalysis (SAPass m) (LDecl lab a) (LDecl lab a) where
    traverseM _ = aaDecl

instance SACtx lab m v => AST.Traversal StructAnalysis (SAPass m) (LExpr lab v) (LExpr lab v) where
    traverseM _ = aaExpr

instance (PassCtx m, Enumerated lab) => AST.Traversal StructAnalysis (SAPass m) (LPat lab) (LPat lab) where
    traverseM _ = aaPat
