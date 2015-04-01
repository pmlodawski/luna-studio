{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

module Luna.Pass.Analysis.Imports where

import           Data.Either                 (lefts, rights)

import           Luna.Data.ModuleInfo        (ImportError(..))
import           Luna.Data.StructInfo        (StructInfo)
import           Luna.Data.ModuleInfo        (_strInfo)
import qualified Luna.Pass.Import            as I
import           Luna.Syntax.Traversals      ()
import           Luna.Pass                    (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass                    as Pass
import           Flowbox.Prelude

data ImportsAnalysis = ImportsAnalysis

--type IAPass                 m   = PassMonad ModuleInfo m
--type IACtx              lab m a = (Enumerated lab, IATraversal m a, MonadIO m)
-- type IATraversal            m a = (PassCtx m, AST.Traversal        ImportAnalysis (IAPass m) a a)
-- type IADefaultTraversal     m a = (PassCtx m, AST.DefaultTraversal ImportAnalysis (IAPass m) a a)


data NoState = NoState deriving (Read, Show)

pass :: (MonadIO m) => Pass NoState (I.ASTUnit l a e -> m ([StructInfo], [ImportError]))
pass = Pass "Import analysis" 
            "Basic import analysis that results in import renaming and proper path" 
            NoState iaMain

iaMain ast = do           
    let impPaths =  I.getImportPaths ast
    infoEithers  <- I.getModuleInfos impPaths
    let mInfos   =  rights infoEithers
        mErrors  =  lefts  infoEithers
        strInfos =  fmap (_strInfo) mInfos
    return (strInfos, mErrors)


