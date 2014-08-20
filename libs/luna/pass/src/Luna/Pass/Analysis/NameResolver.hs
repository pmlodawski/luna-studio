---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TupleSections    #-}

module Luna.Pass.Analysis.NameResolver where

import           Control.Applicative
import           Control.Monad.State hiding (mapM, mapM_)
import qualified Data.List           as List
import           Data.List.Split     (splitOn)
import qualified Data.Maybe          as Maybe

import           Flowbox.Control.Error
import           Flowbox.Prelude           hiding (elements, mod)
import           Flowbox.System.Log.Logger hiding (trace)
import           Luna.AST.Control.Crumbs   (Breadcrumbs)
import qualified Luna.AST.Control.Crumbs   as Crumb
import qualified Luna.AST.Control.Focus    as Focus
import           Luna.AST.Control.Zipper   (Zipper)
import qualified Luna.AST.Control.Zipper   as Zipper
import           Luna.AST.Expr             (Expr)
import qualified Luna.AST.Expr             as Expr
import           Luna.AST.Module           (Module)
import qualified Luna.AST.Module           as Module
import qualified Luna.AST.Type             as Type
import           Luna.Data.Lib.Manager     (LibManager)
import qualified Luna.Data.Lib.Manager     as LibManager
import           Luna.Lib.Lib              (Library)
import qualified Luna.Lib.Lib              as Library
import           Luna.Pass.Pass            (Pass)
import qualified Luna.Pass.Pass            as Pass



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Analysis.NameResolver"


type NRPass result = Pass Pass.NoState result


run :: String -> Breadcrumbs -> Library.ID -> LibManager -> Pass.Result [(Library.ID, Breadcrumbs)]
run = Pass.run_ (Pass.Info "NameResolver") Pass.NoState .:: resolve


resolve :: String -> Breadcrumbs -> Library.ID -> LibManager -> NRPass [(Library.ID, Breadcrumbs)]
resolve name bc libID libManager = do
    library <- LibManager.lab libManager libID <??> "NameResolver: Cannot find library with id=" ++ show libID
    zipper  <- hoistEither $ Zipper.focusCrumb' (head bc) $ library ^. Library.ast
    imports <- getImports zipper $ tail bc
    let elements = splitOn "." name
        possiblePaths = elements
                      : (currentScope bc ++ elements)
                      : mapMaybe (possiblePath elements) imports
    return $ List.concatMap (`searchLibManager` libManager) possiblePaths

getImports :: Zipper -> Breadcrumbs -> NRPass [Expr]
getImports z@(Focus.Module m, _) (h:t) = do newZ <- hoistEither $ Zipper.focusCrumb h z
                                            imports <- getImports newZ t
                                            pure $ (m ^. Module.imports) ++ imports
getImports _                      _    = pure []


possiblePath :: [String] -> Expr -> Maybe [String]
possiblePath elements (Expr.Import _ path (Expr.Con _ name) rename) =
    if imported == head elements
        then Just $ path ++ tail elements
        else Nothing
    where imported = Maybe.fromMaybe name rename


currentScope :: Breadcrumbs -> [String]
currentScope (Crumb.Module   m   : t) = m : currentScope t
currentScope (Crumb.Class    c   : t) = c : currentScope t
currentScope (Crumb.Function _ _ : _) = []
currentScope []                       = []


searchLibManager :: [String] -> LibManager -> [(Library.ID, Breadcrumbs)]
searchLibManager path libManager =
    List.concatMap (\(libID, library) -> (libID,) <$> searchLib path library) $ LibManager.labNodes libManager


searchLib :: [String] -> Library -> [Breadcrumbs]
searchLib path library =
    if libName == head path
        then searchModule path [] ast
        else []
    where libName = library ^. Library.name
          ast     = library ^. Library.ast


-- FIXME: added typeAliases
-- FIXME: added typeDefs
searchModule :: [String] -> Breadcrumbs -> Module -> [Breadcrumbs]
searchModule path bc (Module.Module _ (Type.Module _ name _) _ classes _typeAliases _typeDefs _ methods modules) =
    if not (null path) && name == head path
        then if length path == 1
                then [currentBc]
                else List.concatMap (searchExpr   (tail path) currentBc) (classes ++ methods)
                  ++ List.concatMap (searchModule (tail path) currentBc) modules
        else []
    where currentBc = bc ++ [Crumb.Module $ head path]


searchExpr :: [String] -> Breadcrumbs -> Expr -> [Breadcrumbs]
searchExpr path bc expr = case expr of
    -- TODO [PM] : Add search for functions with path set!
    Expr.Function _ [] name _ _ _           -> if length path == 1 && head path == name
                                               then [bc ++ [Crumb.Function name []]]
                                               else []
    Expr.Data _ (Type.Data _ name _) _ _ _  -> if length path == 1 && head path == name
                                               then [bc ++ [Crumb.Class name]]
                                               else []
    _                                       -> []

