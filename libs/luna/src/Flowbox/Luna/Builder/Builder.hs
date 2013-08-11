---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Builder.Builder(
    Builder(..),
    empty,
    buildLibrary,
    refreshLibrary,
    refreshDef
) where

import qualified Flowbox.Luna.Lib.Library              as Library
import           Flowbox.Luna.Lib.Library                (Library)
import qualified Flowbox.Luna.Network.Def.DefManager  as DefManager
import           Flowbox.Luna.Network.Def.DefManager    (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition  as Definition
import           Flowbox.Luna.Network.Def.Definition    (Definition)
import qualified Flowbox.System.UniPath               as UniPath
import           Flowbox.System.UniPath                 (UniPath)
import qualified Flowbox.Luna.Codegen.Hs.DefGenerator as DG
import qualified Flowbox.Luna.Codegen.Hs.AST.Module   as Module
import qualified Flowbox.Luna.Codegen.Hs.Path         as Path
import qualified Flowbox.System.IO                    as IO
import           Flowbox.System.Directory             as Dir




data Builder = Builder { path :: UniPath }


empty :: Builder
empty = Builder UniPath.empty


buildLibrary :: Builder -> Library -> IO()
buildLibrary bld lib = do
    refreshLibrary bld lib
    --TODO[wd]: wywolanie cabala


refreshLibrary :: Builder -> Library -> IO()
refreshLibrary bld lib = do
    let nodes = DefManager.nodes $ Library.defs lib
    mapM (refreshDef bld lib) nodes
    return ()


refreshDef :: Builder -> Library -> Definition.ID -> IO()
refreshDef bld lib did = do
    let
        hssrcs   = UniPath.append "src"
                 $ UniPath.append "hs"
                 $ UniPath.append (Library.name lib)
                 $ path bld
        mod      = DG.generateDefinition (Library.defs lib) did
        code     = Module.genCode mod
        modpath  = Path.toFilePath $ Module.path mod
        relpath  = UniPath.fromList $ Path.segments modpath
        filepath = hssrcs ++ relpath

    Dir.createDirectoryIfMissing True $ UniPath.dirOf filepath
    IO.writeFile filepath code


--executeShellCommand cmd   = putStrLn ("EXEC: " ++ cmd) >> system cmd >>= check where
--    check (ExitSuccess)   = return ()
--    check (ExitFailure n) = error $ "cmd: " ++ cmd ++ " failure code " ++ show n