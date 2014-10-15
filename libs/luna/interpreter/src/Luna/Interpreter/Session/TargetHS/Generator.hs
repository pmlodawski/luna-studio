---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.TargetHS.Generator where

import qualified Data.List as List

import           Flowbox.Prelude
import           Flowbox.Source.Location                  (loc)
import           Flowbox.System.Log.Logger
import           Luna.AST.Module                          (Module)
import qualified Luna.AST.Module                          as Module
import qualified Luna.AST.Type                            as Type
import qualified Luna.Data.Source                         as Source
import           Luna.Interpreter.Session.Data.DefPoint   (DefPoint)
import qualified Luna.Interpreter.Session.Data.DefPoint   as DefPoint
import           Luna.Interpreter.Session.Session         (Session)
import qualified Luna.Interpreter.Session.Session         as Session
import qualified Luna.Pass.Analysis.Alias.Alias           as Analysis.Alias
import qualified Luna.Pass.CodeGen.HSC.HSC                as HSC
import qualified Luna.Pass.Transform.AST.Hash.Hash        as Hash
import qualified Luna.Pass.Transform.AST.SSA.SSA          as SSA
import qualified Luna.Pass.Transform.HAST.HASTGen.HASTGen as HASTGen



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


emptyModule :: Module
emptyModule = Module.mk def $ Type.Module def "Main" []


genAll :: Session [String]
genAll = do
    mainPtr <- Session.getMainPtr
    ast     <- Session.getModule $ (DefPoint.breadcrumbs %~ init) mainPtr
    genCode (dropWhile (not . (== "-- body --"))) ast


genClass :: DefPoint -> Session [String]
genClass defPoint = do
    expr <- Session.getClass defPoint
    let ast = emptyModule & Module.classes .~ [expr]
    genCode (dropWhile (not . (== "-- body --"))) ast


genFunctions :: Session [String]
genFunctions = do
    --expr <- Session.getFunction defPoint
    --let ast = emptyModule & Module.methods .~ [expr]
    mainPtr <- Session.getMainPtr
    ast     <- Session.getModule $ (DefPoint.breadcrumbs %~ init) mainPtr
    genCode ( List.filter (not . (\a -> List.isPrefixOf "data " a || List.isPrefixOf "$(generateFieldAccessors" a))
             . dropWhile   (not . (== "-- body --"))
            --dropWhile   (not . (== "-- ====== Method: Vector.test ====== --"))

            ) ast


genCode :: ([String] -> [String]) -> Module -> Session [String]
genCode selector ast = do
    aliasInfo <- Session.runPass $(loc) $ Analysis.Alias.run ast
    hash      <- Session.runPass $(loc) $ Hash.run ast
    ssa       <- Session.runPass $(loc) $ SSA.run aliasInfo hash
    hast      <- Session.runPass $(loc) $ HASTGen.run ssa
    srcs      <- Session.runPass $(loc) $ HSC.run hast
    return $ map (unlines . selector . lines . view Source.code) srcs


