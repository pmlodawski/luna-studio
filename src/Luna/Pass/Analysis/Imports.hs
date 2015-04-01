{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

module Luna.Pass.Analysis.Imports where

import           Data.Either                 (isLeft, isRight)
import           Luna.Data.ModuleInfo        (ImportError(..),getStructInfosMap )
import           Luna.Data.StructInfo        (StructInfo)
import           Luna.Data.ModuleInfo        (_strInfo)
import qualified Luna.Pass.Import            as I
import           Luna.Syntax.Traversals      ()
import           Luna.Pass                    (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass                    as Pass
import           Flowbox.Prelude             hiding(isLeft, isRight, filter, map)
import           Luna.Data.ImportInfo        (ImportInfo(ImportInfo))
import           Data.Map                    (elems, filter, map)
import           Data.Either.Utils           (fromRight, fromLeft)
import           Luna.Pass                   (PassMonad)
import qualified Luna.Syntax.Traversals       as AST



data ImportsAnalysis = ImportsAnalysis

type IMPass                 m   = PassMonad NoState m
type IADefaultTraversal     m a = (PassCtx m, AST.DefaultTraversal ImportsAnalysis (IMPass m) a a)

data NoState = NoState deriving (Read, Show)

pass :: (IADefaultTraversal m a) => Pass NoState (I.ASTUnit l a e -> m ImportInfo) 
pass = Pass "Import analysis" 
            "Basic import analysis that results in import renaming and proper path" 
            NoState iaMain

iaMain :: I.ASTUnit l a e -> IO ImportInfo
iaMain ast = do           
    let impPaths =  I.getImportPaths ast
    infoEithers  <- getStructInfosMap impPaths
    let mInfos   =  map fromRight $ filter isRight infoEithers
        mErrors  =  fmap fromLeft $ elems $ filter isLeft infoEithers
    return $ ImportInfo mInfos mempty mErrors   

