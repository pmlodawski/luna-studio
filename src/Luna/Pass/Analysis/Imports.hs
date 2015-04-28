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
import           Luna.Pass                   (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass                   as Pass
import           Flowbox.Prelude             hiding(isLeft, isRight, filter, map, fromList)
import           Luna.Data.ImportInfo        (ImportInfo(ImportInfo), createSymTable)
import           Data.Map                    (elems, filter, map, fromList)
import           Data.Either.Utils           (fromRight, fromLeft)
import           Luna.Pass                   (PassMonad)
import qualified Luna.Syntax.Traversals      as AST
import           Luna.Syntax.Enum            (Enumerated)

import           Luna.Syntax.Unit            (Unit(Unit))
import           Luna.Syntax.Label           (Label(Label), _element)
import           Luna.Syntax.Module          (_body, Module(Module))



data ImportsAnalysis = ImportsAnalysis


data NoState = NoState deriving (Read, Show)

--pass :: (MonadIO m) => Pass NoState (Unit (Label l (Module a e)) -> m ImportInfo)
pass = Pass "Import analysis" 
            "Basic import analysis that performs error checks and returns both struct info and symbol table" 
            NoState iaMain


iaMain :: (MonadIO m) => Unit (Label l (Module a e)) -> m ImportInfo
iaMain ast =  do           
    let impPaths =  I.getImportPaths ast
    listEithers  <- I.moduleInfosToTuples impPaths
    let infoEithers = fromList listEithers
    let mInfos   =  map (_strInfo . fromRight) $ filter isRight infoEithers
        mErrors  =  fmap fromLeft $ elems $ filter isLeft infoEithers
        info     =  ImportInfo mempty (I.getImports ast) mInfos mempty mempty mErrors
    return $ createSymTable info

