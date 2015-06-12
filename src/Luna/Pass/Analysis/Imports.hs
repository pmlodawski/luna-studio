---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2015
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Pass.Analysis.Imports where

import           Control.Monad     (filterM)
import           Data.Either       (isLeft, isRight)
import           Data.Either.Utils (fromLeft, fromRight)
import           Data.Map          (elems, filter, fromList, map)

import           Flowbox.Prelude        hiding (filter, fromList, isLeft, isRight, map)
import           Luna.Data.ImportInfo   (ImportInfo (ImportInfo), createSymTable)
import           Luna.Data.ModuleInfo   (ImportError (..))
import qualified Luna.Data.ModuleInfo   as ModInfo
import           Luna.Data.StructInfo   (StructInfo)
import           Luna.Pass              (Pass (Pass), PassCtx, PassMonad)
import           Luna.Pass              (PassMonad)
import qualified Luna.Pass              as Pass
import qualified Luna.Pass.Import       as Import
import           Luna.Syntax.Enum       (Enumerated)
import           Luna.Syntax.Traversals ()
import qualified Luna.Syntax.Traversals as AST
import           Luna.Syntax.Label  (Label (Label), _element)
import           Luna.Syntax.Module (Module (Module))
import           Luna.Syntax.Unit   (Unit (Unit), ASTUnit)
import           Luna.Syntax.Name.Path (QualPath)


data ImportsAnalysis = ImportsAnalysis


data NoState = NoState deriving (Read, Show)

pass :: MonadIO m => Pass NoState ((ASTUnit a e) -> m (ImportInfo))
pass = Pass "Import analysis"
            "Basic import analysis that performs error checks and returns both struct info and symbol table"
            NoState iaMain


iaMain :: MonadIO m => (ASTUnit a e) -> m (ImportInfo)
iaMain ast =  do
    let impPaths =  Import.getImportPaths ast
    withoutSources <- liftIO $ filterM (\m -> not <$> ModInfo.moduleExists m) impPaths
    listEithers  <- Import.moduleInfosToTuples impPaths
    let infoEithers = fromList listEithers
        mInfos   =  map (ModInfo._strInfo . fromRight) $ filter isRight infoEithers
        mErrors  =  fmap fromLeft $ elems $ filter isLeft infoEithers
        info     =  ImportInfo mempty (Import.getImports ast) mInfos mempty mempty mErrors
    return $ createSymTable info

