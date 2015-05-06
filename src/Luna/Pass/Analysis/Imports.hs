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
import qualified Luna.Data.ModuleInfo   as MI
import           Luna.Data.StructInfo   (StructInfo)
import           Luna.Pass              (Pass (Pass), PassCtx, PassMonad)
import           Luna.Pass              (PassMonad)
import qualified Luna.Pass              as Pass
import qualified Luna.Pass.Import       as I
import           Luna.Syntax.Enum       (Enumerated)
import           Luna.Syntax.Traversals ()
import qualified Luna.Syntax.Traversals as AST
import           Luna.Syntax.Label  (Label (Label), _element)
import           Luna.Syntax.Module (Module (Module))
import           Luna.Syntax.Unit   (Unit (Unit))
import           Luna.Syntax.Name.Path (QualPath)


data ImportsAnalysis = ImportsAnalysis


data NoState = NoState deriving (Read, Show)

pass :: MonadIO m => Pass NoState (Unit (Label l (Module a e)) -> m (ImportInfo))
pass = Pass "Import analysis"
            "Basic import analysis that performs error checks and returns both struct info and symbol table"
            NoState iaMain


iaMain :: MonadIO m => Unit (Label l (Module a e)) -> m (ImportInfo)
iaMain ast =  do
    let impPaths =  I.getImportPaths ast
    withoutSources <- liftIO $ filterM MI.moduleNotExists impPaths
    listEithers  <- I.moduleInfosToTuples impPaths
    let infoEithers = fromList listEithers
        mInfos   =  map (MI._strInfo . fromRight) $ filter isRight infoEithers
        mErrors  =  fmap fromLeft $ elems $ filter isLeft infoEithers
        info     =  ImportInfo mempty (I.getImports ast) mInfos mempty mempty mErrors
    return $ createSymTable info

