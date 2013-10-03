---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Luna.Passes.CodeGen.FClass.Gen where

import           Control.Monad.RWS                           
import qualified Data.List.Split                           as Split

import           Flowbox.Prelude                           hiding (error)
import qualified Flowbox.Luna.Data.Cabal.Config            as CabalConfig
import qualified Flowbox.Luna.Data.Cabal.Section           as CabalSection
import           Flowbox.Luna.Data.Source                    (Source(Source))
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Install as CabalInstall
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Store   as CabalStore
import qualified Flowbox.Luna.Passes.Pass                  as Pass
import           Flowbox.Luna.Passes.Pass                    (PassMonadIO)
import qualified Flowbox.Luna.Passes.Source.File.Writer    as FileWriter
import qualified Flowbox.System.Directory.Directory        as Directory
import           Flowbox.System.Log.Logger                   
import qualified Flowbox.System.UniPath                    as UniPath
import           Flowbox.System.UniPath                      (UniPath)



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Luna.Passes.CodeGen.FClass.Gen"


indent    = replicate 4 ' '
nl        = "\n"
mpostfix  = "''M"
header    = "{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}\n"
modprefix = "FlowboxM.Luna.FClasses"
cprefix   = "C''"
fprefix   = "U_"


genC :: String -> String
genC name = (header ++ nl ++ fhead ++ cls) where
    fname     = name ++ "'"
    cname     = fprefix ++ name
    cfname    = cprefix ++ fname
    fhead     = "module " ++ modprefix ++ "." ++ cname ++ " where\n\n"
    cls       = clsheader ++ clsbody
    clsheader = "class "  ++ cfname ++ " a b | a -> b where\n"
    clsbody   =  indent ++ fname ++          "    :: a -> b\n"
              ++ indent ++ fname ++ mpostfix ++ " :: a -> IO b\n"


-----------------------------------

--run :: MonadIO m => String -> UniPath -> m ()
--run = genAndInstall


genAndInstall :: PassMonadIO s m  => UniPath -> String -> Pass.Result m ()
genAndInstall cabalDevPath name  = do
    let location = "tmp/" ++ name
        fcname   = fprefix ++ name
        source   = Source (Split.splitOn "." modprefix ++ [fcname]) $ genC name
        lib      = CabalSection.mkLibrary { CabalSection.exposedModules = [modprefix ++ "." ++ fcname]}
        cabal    = CabalConfig.addSection lib 
                 $ CabalConfig.make name

        path     = UniPath.append location cabalDevPath
    FileWriter.run (UniPath.append ("src") path) ".hs" source
    CabalStore.run cabal $ UniPath.append (name ++ ".cabal") path
    CabalInstall.run cabalDevPath location
    liftIO $ Directory.removeDirectoryRecursive path
