---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Flowbox.Lunac.Builder.File where

import           Control.Monad.RWS                                     hiding (mapM_)

import           Flowbox.Prelude                                         
import           Flowbox.Config.Config                                   (Config)
import qualified Flowbox.Luna.Data.AST.Module                          as ASTModule
import qualified Flowbox.Luna.Passes.General.Luna.Luna                 as Luna
import qualified Flowbox.Luna.Passes.Pass                              as Pass
import           Flowbox.Luna.Passes.Pass                                (PassMonadIO)
import qualified Flowbox.Luna.Passes.Source.File.Reader                as FileReader
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser as TxtParser
import qualified Flowbox.Lunac.Builder.Builder                         as Builder
import qualified Flowbox.Lunac.CmdArgs                                 as CmdArgs
import           Flowbox.Lunac.CmdArgs                                   (CmdArgs)
import qualified Flowbox.Lunac.Diagnostics                             as Diagnostics
import           Flowbox.Lunac.Diagnostics                               (Diagnostics)
import           Flowbox.System.Log.Logger                               
import qualified Flowbox.System.UniPath                                as UniPath
import           Flowbox.System.UniPath                                  (UniPath)



logger :: Logger
logger = getLogger "Flowbox.Lunac.Builder.File"


build :: Config -> CmdArgs -> Diagnostics -> UniPath -> IO ()
build cfg cmd diag filePath = Luna.runIO $ do 
    let outputPath = UniPath.fromUnixString $ CmdArgs.output cmd
        name       = CmdArgs.libName    cmd
        version    = CmdArgs.libVersion cmd
        rootPath = case CmdArgs.rootPath cmd of 
                        "" -> UniPath.basePath filePath
                        rp -> UniPath.fromUnixString rp
        flags    = case CmdArgs.global cmd of 
                        True  -> ["--global"]
                        False -> []
    ast  <- parseFile diag rootPath filePath
    Builder.build cfg diag outputPath name version (CmdArgs.library cmd) (CmdArgs.link cmd) flags ast


parseFile :: PassMonadIO s m => Diagnostics -> UniPath -> UniPath -> Pass.Result m ASTModule.Module
parseFile diag rootPath filePath = do 
    logger debug $ "Compiling file '" ++ UniPath.toUnixString filePath ++ "'"
    source <- FileReader.run rootPath filePath
    ast    <- TxtParser.run source
    Diagnostics.printAST ast diag 
    return ast
