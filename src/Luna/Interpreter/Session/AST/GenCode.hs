---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.AST.GenCode where

import           Control.Monad                ((>=>))
import           Control.Monad.Trans.Either
import qualified DynFlags                     as GHC
import qualified GhcMonad
import qualified HscTypes
import qualified Language.Haskell.Interpreter as I

import           Flowbox.Prelude
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
logger = getLoggerIO "Luna.Interpreter.Session.GenCode"


enabledFlags :: [GHC.ExtensionFlag]
enabledFlags = [ GHC.Opt_DataKinds
               , GHC.Opt_DeriveDataTypeable
               , GHC.Opt_DeriveGeneric
               , GHC.Opt_DysfunctionalDependencies
               , GHC.Opt_FlexibleContexts
               , GHC.Opt_FlexibleInstances
               , GHC.Opt_GADTs
               , GHC.Opt_RebindableSyntax
               , GHC.Opt_TemplateHaskell
               , GHC.Opt_UndecidableInstances

               , GHC.Opt_MultiParamTypeClasses
               ]


disabledFlags :: [GHC.ExtensionFlag]
disabledFlags = [GHC.Opt_MonomorphismRestriction]


runDecls :: [String] -> Session ()
runDecls = Session.withFlags enabledFlags disabledFlags . mapM_ Session.runDecls


emptyModule :: Module
emptyModule = Module.mk def $ Type.Module def "Main" []


genAll :: Session [String]
genAll = do
    mainPtr <- Session.getMainPtr
    ast     <- Session.getModule $ (DefPoint.breadcrumbs %~ init) mainPtr
    genCode True ast


genClass :: DefPoint -> Session [String]
genClass defPoint = do
    expr <- Session.getClass defPoint
    let ast = emptyModule & Module.classes .~ [expr]
    genCode False ast


genFunction :: DefPoint -> Session [String]
genFunction defPoint = do
    expr <- Session.getFunction defPoint
    let ast = emptyModule & Module.methods .~ [expr]
    genCode False ast


genCode :: Bool -> Module -> Session [String]
genCode wholeCode ast = do
    aliasInfo <- EitherT $ Analysis.Alias.run ast
    hash      <- EitherT $ Hash.run ast
    ssa       <- EitherT $ SSA.run aliasInfo hash
    hast      <- EitherT $ HASTGen.run ssa
    srcs      <- EitherT $ HSC.run hast
    let splitPoint = if wholeCode
            then "-- body --"
            else "-- Other data types"
    return $ map (unlines . dropWhile (not . (== splitPoint)) . lines . view Source.code) srcs


cleanInstances :: Session ()
cleanInstances = lift2 $ I.runGhc $
    -- FIXME [PM] : Code below remove all declared instances. It may be
    --              dangerous and needs to be deeply tested or removed.
    GhcMonad.modifySession $ \hscEnv -> let
        ic = (HscTypes.hsc_IC hscEnv) {HscTypes.ic_instances = ([], []) }
        in hscEnv { HscTypes.hsc_IC = ic}


reloadAll :: Session ()
reloadAll = do
    cleanInstances
    genAll >>= runDecls


reloadFunction :: DefPoint -> Session ()
reloadFunction = genFunction >=> runDecls


reloadClass :: DefPoint -> Session ()
reloadClass = genClass >=> runDecls
