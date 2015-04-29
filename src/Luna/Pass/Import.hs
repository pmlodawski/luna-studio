---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2015
---------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Pass.Import where

--CR[PM->TD] : imports should be splitted in two groups: std and flowbox
import           Control.Monad.IO.Class (liftIO)

--CR[PM->TD] : use qualified import
import           Luna.Syntax.Label      (Label(Label), _element)
--CR[PM->TD] : use qualified import
import           Luna.Syntax.Module     (_mpath, _body, Module(Module))
import qualified Luna.Syntax.Decl       as Dec
import           Luna.Syntax.Name       (TName(TName), VName(VName))
import           Luna.Syntax.Name.Path  (NamePath(NamePath), QualPath(QualPath))
import qualified Luna.Syntax.Name.Path  as NP
import           Luna.Syntax.Unit       (Unit(Unit))

import qualified Luna.Data.ModuleInfo as MI
import qualified Luna.Data.ImportInfo as II
import           Flowbox.Prelude
--CR[PM->TD] : 3 new lines between imports and code


type ASTUnit l a e = Unit (Label l (Module a e))


data TargetInfo = TargetInfo {
--CR[PM->TD] : commas should be on the left and following braces, see other files
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
--CR[PM->TD] : too many newlines (3 instead of 2)



getFromLabel :: Label l a -> a
getFromLabel (Label l a) = a 
--CR[PM->TD] : too many newlines (3 instead of 2)



getModulePath :: ASTUnit l a e -> QualPath
getModulePath ast = _mpath . getFromLabel . getFromUnit $ ast
--CR[PM->TD] : too many newlines (3 instead of 2)



filterImports :: Label l (Dec.Decl a e) -> Bool
filterImports (Label _  (Dec.Imp _))   = True
filterImports _ = False
--CR[PM->TD] : too many newlines (3 instead of 2)



unpackImport :: Dec.Decl a e -> Dec.Imp
unpackImport (Dec.Imp x) = x
--CR[PM->TD] : too many newlines (3 instead of 2)



getImportList :: ASTUnit l a e -> [Dec.Imp]
getImportList = fmap (unpackImport . _element) . filter filterImports . _body . getFromLabel . getFromUnit
--CR[PM->TD] : too many newlines (3 instead of 2)



getModPathsFromImportList :: [Dec.Imp] -> [QualPath]
getModPathsFromImportList list = map getModPath list
--CR[PM->TD] : too many newlines (3 instead of 2)



getModPath :: Dec.Imp -> QualPath
getModPath (Dec.ModImp  path _) = MI.pathToQualPath path
getModPath (Dec.DeclImp path _) = MI.pathToQualPath path
--CR[PM->TD] : too many newlines (3 instead of 2)



processTarget :: Dec.ImpTgt -> TargetInfo
processTarget (Dec.ImpVar  vname vrename) = case NP._base $ unwrap vname of
    "*" -> TargetInfo [] [] True
    _   -> TargetInfo [] [unwrap vname] False
processTarget (Dec.ImpType tname trename) = case NP._base $ unwrap tname of
    "*" -> TargetInfo [] [] True
    _   -> TargetInfo [] [unwrap tname] False 
processTarget (Dec.Wildcard hide)         = TargetInfo (fmap unwrap hide) [] True
--CR[PM->TD] : too many newlines (3 instead of 2)



processTargets :: [Dec.ImpTgt] -> TargetInfo
processTargets targets = mconcat $ fmap processTarget targets


getImport :: Dec.Imp -> II.Import
getImport (Dec.ModImp path rename) = II.Import (MI.pathToQualPath path) False [] [] (fmap unwrap rename)
getImport (Dec.DeclImp path tgts) = II.Import (MI.pathToQualPath path) (tgtInfo ^. wildcard) (tgtInfo ^. hiding) (tgtInfo ^. targets) Nothing
    where tgtInfo = processTargets tgts
--CR[PM->TD] : too many newlines (3 instead of 2)
                                     


getImports :: ASTUnit l a e -> [II.Import]
--CR[PM->TD] : use <$>
getImports ast = fmap getImport $ getImportList ast


getImportPaths :: ASTUnit l a e -> [QualPath]
getImportPaths = getModPathsFromImportList . getImportList
--CR[PM->TD] : too many newlines (3 instead of 2)



--CR[PM->TD] : parenthesis are not required
getModuleInfos :: (MonadIO m) => [QualPath] -> m [Either MI.ImportError MI.ModuleInfo]
getModuleInfos = liftIO . MI.getModuleInfos



--CR[PM->TD] : parenthesis are not required
moduleInfosToTuples :: (MonadIO m) => [QualPath] -> m [(QualPath, Either MI.ImportError MI.ModuleInfo)]
moduleInfosToTuples paths = do
--CR[PM->TD] : use <$>
--CR[PM->TD] : "do" is not required
	eithers <- getModuleInfos paths
	return $ zip paths eithers


getImportInfo :: (MonadIO m) => ASTUnit l a e -> m [(II.Import, Either MI.ImportError MI.ModuleInfo)]
getImportInfo ast = do
    let imports =  getImports ast
        paths   =  getImportPaths ast
--CR[PM->TD] : use <$>
--CR[PM->TD] : "do" is not required
    eithers     <- getModuleInfos paths
    return $ zip imports eithers
--CR[PM->TD] : too many newlines (should be one)



