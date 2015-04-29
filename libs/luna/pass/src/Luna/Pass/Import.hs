---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2015
---------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Pass.Import where

import           Control.Monad.IO.Class (liftIO)

import           Luna.Syntax.Label      (Label(Label))
import qualified Luna.Syntax.Label      as L
import           Luna.Syntax.Module     (Module(Module))
import qualified Luna.Syntax.Module     as M
import qualified Luna.Syntax.Decl       as Dec
import           Luna.Syntax.Name       (TName(TName), VName(VName))
import           Luna.Syntax.Name.Path  (NamePath(NamePath), QualPath(QualPath))
import qualified Luna.Syntax.Name.Path  as NP
import           Luna.Syntax.Unit       (Unit(Unit))

import qualified Luna.Data.ModuleInfo as MI
import qualified Luna.Data.ImportInfo as II
import           Flowbox.Prelude


type ASTUnit l a e = Unit (Label l (Module a e))


data TargetInfo = TargetInfo {
    _hiding   :: [NamePath],
    _targets  :: [NamePath],
    _wildcard :: Bool
}

makeLenses ''TargetInfo

instance Monoid TargetInfo where
    mempty      = TargetInfo [] [] False
    mappend a b = TargetInfo  ((a ^. hiding)   `mappend` (b ^. hiding  ))
                              ((a ^. targets)  `mappend` (b ^. targets ))
                              ((a ^. wildcard) ||        (b ^. wildcard))


getFromUnit :: Unit a -> a
getFromUnit (Unit a) = a



getFromLabel :: Label l a -> a
getFromLabel (Label l a) = a 



getModulePath :: ASTUnit l a e -> QualPath
getModulePath ast = M._mpath . getFromLabel . getFromUnit $ ast



filterImports :: Label l (Dec.Decl a e) -> Bool
filterImports (Label _  (Dec.Imp _))   = True
filterImports _ = False



unpackImport :: Dec.Decl a e -> Dec.Imp
unpackImport (Dec.Imp x) = x



getImportList :: ASTUnit l a e -> [Dec.Imp]
getImportList = fmap (unpackImport . L._element) . filter filterImports . M._body . getFromLabel . getFromUnit



getModPathsFromImportList :: [Dec.Imp] -> [QualPath]
getModPathsFromImportList list = map getModPath list



getModPath :: Dec.Imp -> QualPath
getModPath (Dec.ModImp  path _) = MI.pathToQualPath path
getModPath (Dec.DeclImp path _) = MI.pathToQualPath path



processTarget :: Dec.ImpTgt -> TargetInfo
processTarget (Dec.ImpVar  vname vrename) = case (unwrap vname) ^. NP.base of
    "*" -> TargetInfo [] [] True
    _   -> TargetInfo [] [unwrap vname] False
processTarget (Dec.ImpType tname trename) = case (unwrap tname) ^. NP.base of
    "*" -> TargetInfo [] [] True
    _   -> TargetInfo [] [unwrap tname] False 
processTarget (Dec.Wildcard hide)         = TargetInfo (fmap unwrap hide) [] True



processTargets :: [Dec.ImpTgt] -> TargetInfo
processTargets targets = mconcat $ fmap processTarget targets


getImport :: Dec.Imp -> II.Import
getImport (Dec.ModImp path rename) = II.Import (MI.pathToQualPath path) False [] [] (fmap unwrap rename)
getImport (Dec.DeclImp path tgts) = II.Import (MI.pathToQualPath path) (tgtInfo ^. wildcard) (tgtInfo ^. hiding) (tgtInfo ^. targets) Nothing
    where tgtInfo = processTargets tgts
                                     


getImports :: ASTUnit l a e -> [II.Import]
getImports ast = fmap getImport $ getImportList ast


getImportPaths :: ASTUnit l a e -> [QualPath]
getImportPaths = getModPathsFromImportList . getImportList



getModuleInfos :: (MonadIO m) => [QualPath] -> m [Either MI.ImportError MI.ModuleInfo]
getModuleInfos = liftIO . MI.getModuleInfos



moduleInfosToTuples :: (MonadIO m) => [QualPath] -> m [(QualPath, Either MI.ImportError MI.ModuleInfo)]
moduleInfosToTuples paths = do
	eithers <- getModuleInfos paths
	return $ zip paths eithers


getImportInfo :: (MonadIO m) => ASTUnit l a e -> m [(II.Import, Either MI.ImportError MI.ModuleInfo)]
getImportInfo ast = do
    let imports =  getImports ast
        paths   =  getImportPaths ast
    eithers     <- getModuleInfos paths
    return $ zip imports eithers



