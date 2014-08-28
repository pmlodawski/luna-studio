---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.AST.GenCode where

import           Control.Monad.Trans.Either
import qualified Data.List                  as List
import qualified DynFlags                   as GHC

import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Text.Show.Hs                     (hsShow)
import           Luna.Data.Source                         (Source)
import qualified Luna.Data.Source                         as Source
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


genModule :: Session [Source]
genModule = do
    mainPtr   <- Session.getMainPtr
    ast       <- Session.getModule $ (DefPoint.breadcrumbs %~ init) mainPtr
    aliasInfo <- EitherT $ Analysis.Alias.run ast
    hash      <- EitherT $ Hash.run ast
    ssa       <- EitherT $ SSA.run aliasInfo hash
    hast      <- EitherT $ HASTGen.run ssa
    EitherT $ HSC.run  hast


loadModule :: Session ()
loadModule = do
    srcs <- genModule
    let enableFlags  = [ GHC.Opt_DataKinds
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
        disableFlags = [ GHC.Opt_MonomorphismRestriction
                       ]

    Session.withFlags enableFlags disableFlags $
        mapM_ (Session.runDecls . unlines . dropWhile (not . (== "-- body --")) . lines . view Source.code) srcs
