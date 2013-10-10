---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Luna.Passes.CodeGen.FClass.Gen where

import qualified Data.List.Split                           as Split
import qualified Data.String.Utils                         as StringUtils

import           Flowbox.Prelude                           hiding (error)
import           Flowbox.Config.Config                       (Config)
import           Flowbox.Luna.Data.Source                    (Source(Source))
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Gen     as CabalGen
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Install as CabalInstall
import qualified Flowbox.Luna.Passes.CodeGen.Cabal.Store   as CabalStore
import qualified Flowbox.Luna.Passes.Pass                  as Pass
import           Flowbox.Luna.Passes.Pass                    (PassMonadIO)
import qualified Flowbox.Luna.Passes.Source.File.Writer    as FileWriter
import qualified Flowbox.System.Directory.Directory        as Directory
import           Flowbox.System.Log.Logger                   
import qualified Flowbox.System.UniPath                    as UniPath



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Luna.Passes.CodeGen.FClass.Gen"


pprefix   = "flowboxM-FClasses-"
indent    = replicate 4 ' '
nl        = "\n"
header    = "{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}\n"
modprefix = "FlowboxM.Luna.FClasses"

mprefix   = "U_"
cprefix   = "FC_"
iprefix   = "_"
csaprefix = "Arg_"
isaprefix = "setArg_"

version = "0.0"

genCode :: String -> String
genCode name = (header ++ nl ++ fhead ++ cls ++ sacls) where
    fhead        = "module " ++ modprefix ++ "." ++ mprefix ++ name ++ " where\n\n"
    cls          = cls_header ++ cls_body
    cls_header   = "class " ++ cprefix ++ name ++ " a b | a -> b where\n"
    cls_body     = indent   ++ iprefix ++ name ++ " :: a -> b\n"
    sacls        = sacls_header ++ sacls_body
    sacls_header = "class " ++ csaprefix ++ name ++ " m a b | m -> a, m a -> b where\n"
    sacls_body   = indent   ++ isaprefix ++ name ++ " :: m a -> b\n"


-----------------------------------

--run :: MonadIO m => String -> UniPath -> m ()
--run = genAndInstall

packageName :: String -> String
packageName name = pprefix ++ clearedname where
    clearedname = StringUtils.replace "_" ("-"++safePrefix)
                $ StringUtils.replace "-" ("-"++safePrefix)
                $ StringUtils.replace safePrefix (safePrefix++safePrefix) name
    safePrefix  = "SAFEPREFIX"


genAndInstall :: PassMonadIO s m  => Config -> String -> [String] -> Pass.Result m ()
genAndInstall config name flags = do
    let fcname   = mprefix ++ name
        source   = Source (Split.splitOn "." modprefix ++ [fcname]) $ genCode name
        cabal    = CabalGen.genLibrary [source] (packageName name) version []

    Directory.withTmpDirectory "luna" (\tmpDir -> do
        FileWriter.run (UniPath.append ("src") tmpDir) ".hs" source
        CabalStore.run cabal $ UniPath.append (name ++ ".cabal") tmpDir
        CabalInstall.run config tmpDir flags)

