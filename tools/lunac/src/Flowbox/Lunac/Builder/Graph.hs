---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoMonomorphismRestriction, ScopedTypeVariables #-}

module Flowbox.Lunac.Builder.Graph where

import           Control.Monad.RWS                                         hiding (mapM_)
import           Data.Maybe                                                as Maybe

import           Flowbox.Prelude                                             
import           Flowbox.Config.Config                                       (Config)
import qualified Flowbox.Luna.Data.AST.Module                              as ASTModule
import qualified Flowbox.Luna.Lib.Library                                  as Library
import           Flowbox.Luna.Lib.Library                                    (Library)
import qualified Flowbox.Luna.Network.Def.Definition                       as Definition
import           Flowbox.Luna.Network.Def.Definition                         (Definition)
import qualified Flowbox.Luna.Network.Def.DefManager                       as DefManager
import           Flowbox.Luna.Network.Def.DefManager                         (DefManager)
import qualified Flowbox.Luna.Passes.General.Luna.Luna                     as Luna
import qualified Flowbox.Luna.Passes.Pass                                  as Pass
import           Flowbox.Luna.Passes.Pass                                    (PassMonad)
import qualified Flowbox.Luna.Passes.Transform.AST.GraphParser.GraphParser as GraphParser
import qualified Flowbox.Lunac.Builder.Builder                             as Builder
import qualified Flowbox.Lunac.Diagnostics                                 as Diagnostics
import           Flowbox.Lunac.Diagnostics                                   (Diagnostics)
import           Flowbox.System.Log.Logger                                   
import           Flowbox.System.UniPath                                      (UniPath)
import qualified Flowbox.Text.Show.Pretty                                  as PP



logger :: Logger
logger = getLogger "Flowbox.Lunac.Builder.Graph"


build :: Config -> Diagnostics -> Library -> String -> UniPath -> IO ()
build config diag library name outputPath = Luna.runIO $ do
    let defManger = Library.defs library
        rootDefID = Library.rootDefID
        rootDef   = Maybe.fromJust $ DefManager.lab defManger rootDefID
    ast     <- parseGraph diag defManger (rootDefID, rootDef)
    Builder.build config diag outputPath name False [] ast



parseGraph :: PassMonad s m => Diagnostics -> DefManager -> (Definition.ID, Definition) -> Pass.Result m ASTModule.Module
parseGraph diag defManager def = do 
    logger debug "Compiling graph"
    logger info (PP.ppShow  defManager)
    ast <- GraphParser.run defManager def
    Diagnostics.printAST ast diag 
    return ast
