---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Flowbox.Luna.Passes.Analysis.NameResolver where

import           Control.Applicative
import           Control.Monad.State hiding (mapM, mapM_)
import qualified Data.List           as List
import           Data.Maybe          (mapMaybe)

import           Data.List.Split                         (splitOn)
import           Flowbox.Control.Error
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs (Breadcrumbs)
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb       as Crumb
import           Flowbox.Luna.Data.AST.Expr              (Expr)
import qualified Flowbox.Luna.Data.AST.Expr              as Expr
import           Flowbox.Luna.Data.AST.Module            (Module)
import qualified Flowbox.Luna.Data.AST.Module            as Module
import           Flowbox.Luna.Data.AST.Type              (Type)
import qualified Flowbox.Luna.Data.AST.Type              as Type
import qualified Flowbox.Luna.Data.AST.Zipper.Focus      as Focus
import           Flowbox.Luna.Data.AST.Zipper.Zipper     (Zipper)
import qualified Flowbox.Luna.Data.AST.Zipper.Zipper     as Zipper
import           Flowbox.Luna.Lib.LibManager             (LibManager)
import qualified Flowbox.Luna.Lib.LibManager             as LibManager
import           Flowbox.Luna.Lib.Library                (Library)
import qualified Flowbox.Luna.Lib.Library                as Library
import           Flowbox.Prelude                         hiding (Zipper, elements, focus, mod, zipper)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Analysis.NameResolver"


getImports :: (Applicative m, Monad m) => Zipper -> Breadcrumbs -> m [Expr]
getImports z@(Focus.ModuleFocus m, _) (h:t) = do newZ <- Zipper.focusCrumb h z
                                                 imports <- getImports newZ t
                                                 pure $ (m ^. Module.imports) ++ imports
getImports _                              _ = pure []


resolve :: (Applicative m, Monad m)
        => String -> Breadcrumbs -> Library.ID -> LibManager -> m [(Breadcrumbs, Library.ID)]
resolve name bc libID libManager = do
    library <- LibManager.lab libManager libID <?> "NameResolver: Cannot find library with id=" ++ show libID
    zipper  <- Zipper.mk $ Library.ast $ library
    imports <- getImports zipper bc
    let elements = splitOn "." name
        possiblePaths = mapMaybe (possiblePath elements) imports
    return $ List.concat $ map (flip searchLibManager libManager) $ elements:possiblePaths


--possiblePaths :: String -> [Expr] -> [String]
--possiblePaths name imports = paths where
--    elements = splitOn "." name


possiblePath :: [String] -> Expr -> Maybe [String]
possiblePath elements (Expr.Import _ path (Expr.Var _ name) rename) =
    if imported == head elements
        then Just $ path ++ tail elements
        else Nothing
    where imported = case rename of
                        Nothing -> name
                        Just r  -> r


searchLibManager :: [String] -> LibManager -> [(Breadcrumbs, Library.ID)]
searchLibManager path libManager = do
    mapMaybe (\(libID, library) -> (,libID) <$> searchLib path library) $ LibManager.labNodes libManager


searchLib :: [String] -> Library -> Maybe Breadcrumbs
searchLib path library =
    if libName == head path
        then searchModule (tail path) [] ast
        else Nothing
    where libName = Library.name library
          ast     = Library.ast  library


searchModule :: [String] -> Breadcrumbs -> Module -> Maybe Breadcrumbs
searchModule path bc mod = undefined


searchExpr :: [String] -> Breadcrumbs -> Expr -> Maybe Breadcrumbs
searchExpr path bc expr = case expr of
    Expr.Function    _ _ name _ _ _ -> if length path == 1 && head path == name
                                          then Just $ bc ++ [Crumb.FunctionCrumb name]
                                          else Nothing
    Expr.Class       _ cls _ _ _    -> searchType path bc cls
    _                               -> Nothing


searchType :: [String] -> Breadcrumbs -> Type -> Maybe Breadcrumbs
searchType path bc t = case t of
    Type.Class _ name _ -> if length path == 1 && head path == name
                               then Just $ bc ++ [Crumb.ClassCrumb name]
                               else Nothing
    Type.Module _ name  -> if length path == 1 && head path == head name
                               then Just $ bc ++ [Crumb.ModuleCrumb $ head name]
                               else Nothing
    _                   -> Nothing
