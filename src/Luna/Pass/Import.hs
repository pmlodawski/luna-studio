{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Pass.Import where

import           Control.Monad.IO.Class (liftIO)

import           Luna.Syntax.Label      (Label(Label), _element)
import           Luna.Syntax.Module     (_mpath, _body, Module(Module))
import qualified Luna.Syntax.Decl       as Dec
import           Luna.Syntax.Name       (TName(TName))
import           Luna.Syntax.Name.Path  (QualPath(QualPath), multi)
import           Luna.Syntax.Unit       (Unit(Unit))

import qualified Luna.Data.ModuleInfo as MI
import qualified Luna.Data.ImportInfo as II

import           Flowbox.Prelude


type ASTUnit l a e = Unit (Label l (Module a e))


getFromUnit :: Unit a -> a
getFromUnit (Unit a) = a



getFromLabel :: Label l a -> a
getFromLabel (Label l a) = a 



getModulePath :: ASTUnit l a e -> QualPath
getModulePath ast = _mpath . getFromLabel . getFromUnit $ ast



filterImports :: Label l (Dec.Decl a e) -> Bool
filterImports (Label _  (Dec.Imp _))   = True
filterImports _ = False



unpackImport :: Dec.Decl a e -> Dec.Imp
unpackImport (Dec.Imp x) = x



getImportList :: ASTUnit l a e -> [Dec.Imp]
getImportList = fmap (unpackImport . _element) . filter filterImports . _body . getFromLabel . getFromUnit



getModPathsFromImportList :: [Dec.Imp] -> [QualPath]
getModPathsFromImportList list = map getModPath list



getModPath :: Dec.Imp -> QualPath
getModPath (Dec.ModImp  path _) = MI.pathToQualPath path
getModPath (Dec.DeclImp path _) = MI.pathToQualPath path



getImport :: Dec.Imp -> II.Import
getImport = undefined


getImports :: ASTUnit l a e -> [II.Import]
getImports = undefined


getImportPaths :: ASTUnit l a e -> [QualPath]
getImportPaths = getModPathsFromImportList . getImportList



getModuleInfos :: (MonadIO m) => [QualPath] -> m [Either MI.ImportError MI.ModuleInfo]
getModuleInfos = liftIO . MI.getModuleInfos



moduleInfosToTuples :: (MonadIO m) => [QualPath] -> m [(QualPath, Either MI.ImportError MI.ModuleInfo)]
moduleInfosToTuples paths = do
	eithers <- getModuleInfos paths
	return $ zip paths eithers


