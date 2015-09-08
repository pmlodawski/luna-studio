---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2015
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Luna.Pass.Import where

import Control.Monad.IO.Class (liftIO)

import qualified Luna.Syntax.Decl      as Decl
import           Luna.Syntax.Label     (Label (Label))
import qualified Luna.Syntax.Label     as L
import           Luna.Syntax.Module    (Module (Module))
import qualified Luna.Syntax.Module    as M
import           Luna.Syntax.Name      (TName (TName), VName (VName))
import           Luna.Syntax.Name.Path (NamePath (NamePath), QualPath (QualPath))
import qualified Luna.Syntax.Name.Path as NP
import           Luna.Syntax.Unit      (Unit (Unit), ASTUnit)
import qualified Luna.Syntax.Unit      as Unit

import           Flowbox.Prelude
import qualified Luna.Data.ImportInfo as ImpInfo
import qualified Luna.Data.ModuleInfo as ModInfo


data TargetInfo = TargetInfo { _hiding   :: [NamePath]
                             , _targets  :: [NamePath]
                             , _wildcard :: Bool
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


getModulePath :: ASTUnit a e -> QualPath
getModulePath ast = M._mpath . getFromLabel . getFromUnit $ ast



unpackImport :: Decl.Decl a e -> Decl.Imp
unpackImport (Decl.Imp x) = x






processTarget :: Decl.ImpTgt -> TargetInfo
processTarget (Decl.ImpVar  vname vrename) = case (unwrap vname) ^. NP.base of
    "*" -> TargetInfo [] [] True
    _   -> TargetInfo [] [unwrap vname] False
processTarget (Decl.ImpType tname trename) = case (unwrap tname) ^. NP.base of
    "*" -> TargetInfo [] [] True
    _   -> TargetInfo [] [unwrap tname] False
processTarget (Decl.Wildcard hide)         = TargetInfo (fmap unwrap hide) [] True


processTargets :: [Decl.ImpTgt] -> TargetInfo
processTargets targets = mconcat $ fmap processTarget targets


getImport :: Decl.Imp -> ImpInfo.Import
getImport (Decl.ModImp path rename) = ImpInfo.Import (pathToQualPath path) False [] [] (fmap unwrap rename)
getImport (Decl.DeclImp path tgts) = ImpInfo.Import (pathToQualPath path) (tgtInfo ^. wildcard) (tgtInfo ^. hiding) (tgtInfo ^. targets) Nothing
    where tgtInfo = processTargets tgts


getImports :: ASTUnit a e -> [ImpInfo.Import]
getImports ast = getImport <$> Unit.imports ast


getImportPaths :: ASTUnit a e -> [QualPath]
getImportPaths = fmap (pathToQualPath . view Decl.modPath) . Unit.imports


getModuleInfos :: MonadIO m => [QualPath] -> m [Either ModInfo.ImportError ModInfo.ModuleInfo]
getModuleInfos = liftIO . ModInfo.getModuleInfos



moduleInfosToTuples :: (MonadIO m) => [QualPath] -> m [(QualPath, Either ModInfo.ImportError ModInfo.ModuleInfo)]
moduleInfosToTuples paths = do
  eithers <- getModuleInfos paths
  return $ zip paths eithers


getImportInfo :: (MonadIO m) => ASTUnit a e -> m [(ImpInfo.Import, Either ModInfo.ImportError ModInfo.ModuleInfo)]
getImportInfo ast = do
    let imports =  getImports ast
        paths   =  getImportPaths ast
    eithers     <- getModuleInfos paths
    return $ zip imports eithers


pathToQualPath :: Decl.Path -> QualPath
pathToQualPath = fromList . fmap (toText . unwrap)
--    where list = map toText path
--          n    = last list
--          ns   = init list

--qualPathToPath :: QualPath -> Path
--qualPathToPath (QualPath ns n) = segs ++ [seg]
--    where segs = map makeTNameP ns
--          seg  = (fromText n) :: TNameP
--          makeTNameP = (\x -> fromText x) :: T.Text -> TNameP