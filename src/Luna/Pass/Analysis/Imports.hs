{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Pass.Analysis.Imports where

import           Data.Either                 (isLeft, isRight)
import           Luna.Data.ModuleInfo        (ImportError(..))
import           Luna.Data.StructInfo        (StructInfo)
import           Luna.Data.ModuleInfo        (_strInfo)
import qualified Luna.Pass.Import            as I
import           Luna.Syntax.Traversals      ()
import           Luna.Pass                    (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass                    as Pass
import           Flowbox.Prelude             hiding(isLeft, isRight, filter, map, fromList)
import           Luna.Data.ImportInfo        (ImportInfo(ImportInfo))
import           Data.Map                    (elems, filter, map, fromList)
import           Data.Either.Utils           (fromRight, fromLeft)
import           Luna.Pass                   (PassMonad)
import qualified Luna.Syntax.Traversals       as AST
import           Luna.Syntax.Enum             (Enumerated)

import           Luna.Syntax.Unit       (Unit(Unit))
import           Luna.Syntax.Label      (Label(Label), _element)
import           Luna.Syntax.Module     (_body, Module(Module))



data ImportsAnalysis = ImportsAnalysis

type IMPass                 m   = PassMonad NoState m
type IMDefaultTraversal     m a = (PassCtx m, AST.DefaultTraversal ImportsAnalysis (IMPass m) a a)
type IMCtx              lab m a = (Enumerated lab, IMTraversal m a, MonadIO m)
type IMTraversal            m a = (PassCtx m, AST.Traversal        ImportsAnalysis (IMPass m) a a)

data NoState = NoState deriving (Read, Show)

defaultTraverseM :: (IMDefaultTraversal m a) => a -> IMPass m a
defaultTraverseM = AST.defaultTraverseM ImportsAnalysis

--pass :: (MonadIO m) => Pass NoState (Unit (Label l (Module a e)) -> m ImportInfo)
pass = Pass "Import analysis" 
            "Basic import analysis that results in import renaming and proper path" 
            NoState iaMain

iaMain :: (MonadIO m) => Unit (Label l (Module a e)) -> m ImportInfo
iaMain ast =  do           
    let impPaths =  I.getImportPaths ast
    listEithers  <- I.moduleInfosToTuples impPaths
    let infoEithers = fromList listEithers
    let mInfos   =  map (_strInfo . fromRight) $ filter isRight infoEithers
        mErrors  =  fmap fromLeft $ elems $ filter isLeft infoEithers
    return $ ImportInfo mInfos mempty mErrors   

