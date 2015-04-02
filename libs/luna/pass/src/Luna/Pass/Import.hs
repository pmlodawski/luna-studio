{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Pass.Import where

import           Control.Monad.IO.Class (liftIO)

import           Luna.Syntax.Label      (Label(Label), _element)
import           Luna.Syntax.Module     (_body, Module(Module))
import qualified Luna.Syntax.Decl       as Dec
import           Luna.Syntax.Name       (TName(TName))
import           Luna.Syntax.Unit       (Unit(Unit))

import qualified Luna.Data.ModuleInfo as MI

import           Flowbox.Prelude


type ASTUnit l a e = Unit (Label l (Module a e))


getFromUnit :: Unit a -> a
getFromUnit (Unit a) = a



getFromLabel :: Label l a -> a
getFromLabel (Label l a) = a 



filterImports :: Label l (Dec.Decl a e) -> Bool
filterImports (Label _  (Dec.Imp _))   = True
filterImports _ = False



unpackImport :: Dec.Decl a e -> Dec.Imp
unpackImport (Dec.Imp x) = x



getImportList :: ASTUnit l a e -> [Dec.Imp]
getImportList = fmap (unpackImport . _element) . filter filterImports . _body . getFromLabel . getFromUnit



getModPathsFromImportList :: [Dec.Imp] -> [Dec.Path]
getModPathsFromImportList list = map getModPath list



getModPath :: Dec.Imp -> Dec.Path
getModPath (Dec.ModImp  path _) = path
getModPath (Dec.DeclImp path _) = path



getImportPaths :: ASTUnit l a e -> [Dec.Path]
getImportPaths = getModPathsFromImportList . getImportList



getModuleInfos :: (MonadIO m) => [Dec.Path] -> m [Either MI.ImportError MI.ModuleInfo]
getModuleInfos = liftIO . MI.getModuleInfos



moduleInfosToTuples :: (MonadIO m) => [Dec.Path] -> m [(Dec.Path, Either MI.ImportError MI.ModuleInfo)]
moduleInfosToTuples paths = do
	eithers <- getModuleInfos paths
	return $ zip paths eithers




