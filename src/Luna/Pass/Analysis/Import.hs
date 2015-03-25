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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Luna.Pass.Analysis.Import where

import           Flowbox.Prelude
import           Luna.Data.StructInfo (StructInfo)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Luna.Syntax.Traversals       as AST
--import qualified Luna.Syntax.Enum             as Enum
import           Luna.Syntax.Enum             (Enumerated, IDTag(IDTag))
--import qualified Luna.Syntax.Decl             as Decl
--import           Luna.Syntax.Decl             (LDecl, Field(Field))
--import qualified Luna.Syntax.Module           as Module
--import           Luna.Syntax.Module           (Module(Module), LModule)
--import           Luna.Syntax.Unit             (Unit(Unit))
import qualified Luna.Syntax.Label            as Label
import           Luna.Syntax.Label            (Label(Label))
--import qualified Luna.Syntax.Type             as Type
--import           Luna.Syntax.Type             (Type)
import           Luna.Syntax.Expr             (LExpr, Expr)
import qualified Luna.Syntax.Expr             as Expr
--import qualified Luna.Syntax.Pat              as Pat
--import           Luna.Syntax.Pat              (LPat, Pat)
--import qualified Luna.Syntax.Lit              as Lit 
--import           Luna.Syntax.Arg              (Arg(Arg))
--import qualified Luna.Syntax.Native           as Native
--import           Luna.Syntax.Name.Path        (NamePath(NamePath))
--import qualified Luna.Syntax.Name.Path        as NamePath
--import qualified Luna.Syntax.Name             as Name
--import           Luna.Syntax.Name             (TName(TName), TVName(TVName))
import           Luna.Pass                    (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass                    as Pass
--
import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)
--
--import           Luna.Data.StructInfo         (StructInfo, OriginInfo(OriginInfo))
--import qualified Luna.Data.StructInfo         as SI
--import qualified Luna.Data.ModuleInfo         as ModuleInfo
--
--import qualified Luna.Data.Namespace.State    as State 
--import           Luna.Data.Namespace.State    (regAlias, regOrphan, regParent, regVarName, regNamePatDesc, regTypeName, withScope)
--import qualified Luna.Parser.State            as ParserState
----import qualified Luna.Syntax.Name.Pattern     as NamePattern
--import qualified Luna.Syntax.Name.Pattern     as NamePattern
--import           Luna.Syntax.Foreign          (Foreign(Foreign))

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data ImportAnalysis = ImportAnalysis

type IAPass                 m   = PassMonad Namespace m
type IACtx              lab m a = (Enumerated lab, IATraversal m a, MonadIO m)
type IATraversal            m a = (PassCtx m, AST.Traversal        ImportAnalysis (IAPass m) a a)
type IADefaultTraversal     m a = (PassCtx m, AST.DefaultTraversal ImportAnalysis (IAPass m) a a)

----------------------------------------------------------------------
-- Utils functions
----------------------------------------------------------------------

traverseM :: (IATraversal m a) => a -> IAPass m a
traverseM = AST.traverseM ImportAnalysis

defaultTraverseM :: (IADefaultTraversal m a) => a -> IAPass m a
defaultTraverseM = AST.defaultTraverseM ImportAnalysis

----------------------------------------------------------------------
-- Pass functions
----------------------------------------------------------------------

pass :: (Monoid s, IADefaultTraversal m a) => Pass s (a -> IAPass m StructInfo)
pass = Pass "Import analysis" 
            "Basic import analysis that results in import renaming and proper path" 
            mempty iaUnit

iaUnit :: IADefaultTraversal m a => a -> IAPass m StructInfo
iaUnit ast = defaultTraverseM ast *> (view Namespace.info <$> get)

iaExpr :: IACtx lab m a => LExpr lab a -> IAPass m (LExpr lab a)
iaExpr e@(Label id expr) = case expr of
                  Expr.Var ident -> (putStrLn "blee") *> continue

                  _              -> continue
              where continue = defaultTraverseM e 



-- putStrLn "AAA" *> defaultTraverseM expr






instance IACtx lab m a => AST.Traversal ImportAnalysis (IAPass m) (LExpr lab a) (LExpr lab a) where
    traverseM _ = iaExpr

-- aaMod :: SACtx lab m a => LModule lab a -> SAPass m (LModule lab a)
-- aaMod mod@(Label lab (Module _ body)) = withScope id continue
--     where continue =  registerDecls body
--                    *> defaultTraverseM mod
--           id       = Enum.id lab

-- aaPat :: (PassCtx m, Enumerated lab) => LPat lab -> SAPass m (LPat lab)
-- aaPat p@(Label lab pat) = case pat of
--     Pat.Var         name       -> regVarName (OriginInfo "dupa" id) (unwrap name)
--                                   *> regParent id
--                                   *> continue
--     _                          -> continue
--     where id = Enum.id lab
--           continue = defaultTraverseM p 

-- aaDecl :: SACtx lab m a => (LDecl lab a) -> SAPass m (LDecl lab a)
-- aaDecl d@(Label lab decl) = withScope id continue
--     where id       = Enum.id lab
--           continue = defaultTraverseM d

-- aaExpr :: SACtx lab m a => (LExpr lab a) -> SAPass m (LExpr lab a)
-- aaExpr e@(Label lab expr) = case expr of
--     var@(Expr.Var (Expr.Variable name _)) -> regParent id
--                                           *> regAlias id (unwrap name)
--                                           *> continue
--     cons@(Expr.Cons name)                 -> regParent id
--                                           *> regAlias id (unwrap name)
--                                           *> continue
--     Expr.RecUpd name _                    -> regParent  id
--                                           *> regAlias   id (unwrap name)
--                                           *> regVarName (OriginInfo "dupa" id) (unwrap name)
--                                           *> continue
--     _                                     -> continue
--     where id       = Enum.id lab
--           continue = defaultTraverseM e


-- registerDecls :: SACtx lab m a => [LDecl lab a] -> SAPass m ()
-- registerDecls decls =  mapM_ registerHeaders  decls
--                     *> mapM_ registerDataDecl decls

-- registerDataDecl :: SACtx lab m a => LDecl lab a -> SAPass m ()
-- registerDataDecl (Label lab decl) = case decl of
--     Decl.Data (Decl.DataDecl name _ cons defs) -> withScope id $ do
--         mapM_ registerCons cons
--         registerDecls defs
--         pure ()
--     _                                          -> pure ()
--     where id = Enum.id lab
--           registerCons (Label lab (Decl.Cons _ fields)) = mapM registerField fields
--           registerField (Label lab (Decl.Field t mn v)) = case mn of
--               Nothing -> return ()
--               Just n  -> regVarName (OriginInfo "dupa" (Enum.id lab)) (unwrap n)


-- registerHeaders :: SACtx lab m a => LDecl lab a -> SAPass m ()
-- registerHeaders (Label lab decl) = case decl of
--     Decl.Func    fdecl -> regFuncDecl id fdecl
--     Decl.Data    ddecl -> regDataDecl id ddecl
--     Decl.Foreign fdecl -> regForeignDecl id fdecl
--     Decl.Imp     imp   -> regImport id imp
--     _                  -> pure ()
--     where id = Enum.id lab
          
-- regForeignDecl id (Foreign tgt fdecl) = case fdecl of
--     Decl.FData ddecl -> regDataDecl id ddecl
--     Decl.FFunc fdecl -> regFuncDecl id fdecl
--     _                -> pure ()

-- regImport id imp = do
--     let path = Decl._modPath imp
--     exists <- liftIO $ ModuleInfo.moduleExists path
--     unless exists $ regOrphan id (SI.ImportError path "Module not found.")
    



-- -- regParent here means that the class will be the parent of its methods. Do we need it? Seems sane. 
-- regFuncDecl id (Decl.FuncDecl _ sig _ _) = regParent id *> regVarName (OriginInfo "dupa" id) (NamePattern.toNamePath sig)
--                                          <* regNamePatDesc id (NamePattern.toDesc sig)
-- regDataDecl id (Decl.DataDecl name _ cons _) = regParent id *> regTypeName (OriginInfo "dupa" id) (unwrap name) 
--                                              <* mapM_ registerCons cons
--     where registerCons (Label lab (Decl.Cons name fields)) = regVarName (OriginInfo "dupa" (Enum.id lab)) (unwrap name)

-- ----------------------------------------------------------------------
-- -- Instances
-- ----------------------------------------------------------------------


-- instance SACtx lab m v => AST.Traversal StructAnalysis (SAPass m) (LExpr lab v) (LExpr lab v) where
--     traverseM _ = aaExpr

-- instance (PassCtx m, Enumerated lab) => AST.Traversal StructAnalysis (SAPass m) (LPat lab) (LPat lab) where
--     traverseM _ = aaPat