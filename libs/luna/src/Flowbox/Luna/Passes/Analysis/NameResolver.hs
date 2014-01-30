---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Flowbox.Luna.Passes.Analysis.NameResolver where

import           Control.Applicative
import           Control.Monad.State hiding (mapM, mapM_)
import qualified Data.List           as List

import           Data.List.Split                         (splitOn)
import           Flowbox.Control.Error
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs (Breadcrumbs)
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb       as Crumb
import           Flowbox.Luna.Data.AST.Expr              (Expr)
import qualified Flowbox.Luna.Data.AST.Expr              as Expr
import           Flowbox.Luna.Data.AST.Module            (Module)
import qualified Flowbox.Luna.Data.AST.Module            as Module
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


resolve :: (Applicative m, Monad m)
        => String -> Breadcrumbs -> Library.ID -> LibManager -> m [(Breadcrumbs, Library.ID)]
resolve name bc libID libManager = do
    library <- LibManager.lab libManager libID <?> "NameResolver: Cannot find library with id=" ++ show libID
    zipper  <- Zipper.focusCrumb' (head bc) $ Library.ast $ library
    imports <- getImports zipper $ tail bc
    let elements = splitOn "." name
        possiblePaths = elements
                      : (currentScope bc ++ elements)
                      : (mapMaybe (possiblePath elements) imports)
    return $ List.concat $ map (flip searchLibManager libManager) possiblePaths

getImports :: (Applicative m, Monad m) => Zipper -> Breadcrumbs -> m [Expr]
getImports z@(Focus.ModuleFocus m, _) (h:t) = do newZ <- Zipper.focusCrumb h z
                                                 imports <- getImports newZ t
                                                 pure $ (m ^. Module.imports) ++ imports
getImports _                           _    = pure []


possiblePath :: [String] -> Expr -> Maybe [String]
possiblePath elements (Expr.Import _ path (Expr.Con _ name) rename) =
    if imported == head elements
        then Just $ path ++ tail elements
        else Nothing
    where imported = case rename of
                        Nothing -> name
                        Just r  -> r


currentScope :: Breadcrumbs -> [String]
currentScope ((Crumb.ModuleCrumb   m):t) = m:(currentScope t)
currentScope ((Crumb.ClassCrumb    c):t) = c:(currentScope t)
currentScope ((Crumb.FunctionCrumb _):_) = []
currentScope []                          = []


searchLibManager :: [String] -> LibManager -> [(Breadcrumbs, Library.ID)]
searchLibManager path libManager = do
    List.concat $ map (\(libID, library) -> (,libID) <$> searchLib path library) $ LibManager.labNodes libManager


searchLib :: [String] -> Library -> [Breadcrumbs]
searchLib path library =
    if libName == head path
        then searchModule path [] ast
        else []
    where libName = Library.name library
          ast     = Library.ast  library


-- FIXME: added typeAliases
-- FIXME: added typeDefs
searchModule :: [String] -> Breadcrumbs -> Module -> [Breadcrumbs]
searchModule path bc (Module.Module _ (Type.Module _ name _) _ classes _typeAliases _typeDefs _ methods modules) =
    if length path > 0 && name == head path
        then if length path == 1
                then [currentBc]
                else (List.concat $ map (searchExpr   (tail path) currentBc) $ classes ++ methods)
                  ++ (List.concat $ map (searchModule (tail path) currentBc) modules)
        else []
    where currentBc = bc ++ [Crumb.ModuleCrumb $ head path]


searchExpr :: [String] -> Breadcrumbs -> Expr -> [Breadcrumbs]
searchExpr path bc expr = case expr of
    Expr.Function _ _ name _ _ _        -> if length path == 1 && head path == name
                                              then [bc ++ [Crumb.FunctionCrumb name]]
                                              else []
    Expr.Data _ (Type.Data _ name _) _ _ _ -> if length path == 1 && head path == name
                                              then [bc ++ [Crumb.ClassCrumb name]]
                                              else []
    _                                   -> []

