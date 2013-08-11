---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Luna.Builder.Builder(
    Builder(..),
    empty,
    buildLibrary
) where

import qualified Flowbox.Luna.Lib.Library              as Library
import           Flowbox.Luna.Lib.Library                (Library)
import qualified Flowbox.System.IO                    as IO
import           Flowbox.System.Directory             as Dir
import qualified Flowbox.System.UniPath               as UniPath
import           Flowbox.System.UniPath                 (UniPath)



data Builder = Builder { path :: UniPath }


empty :: Builder
empty = Builder UniPath.empty

--store :: Library -> IO ()
--store lib = do 
--    let
--        nodes = DefManager.nodes $ defs lib
--    mapM (storeDef lib) nodes
--    return ()

--storeDef :: Library -> Definition.ID -> IO ()
--storeDef lib did = do
--    let mod  = DG.generateDefinition (defs lib) did
--        code = Module.genCode mod
--        modpath = Path.toFilePath $ Module.path mod
--        modupath = UniPath.fromList $ Path.segments modpath
--        path' = (path lib) ++ modupath

--    Dir.createDirectoryIfMissing True $ UniPath.dirOf path'
--    IO.writeFile path' code
--    return ()


buildLibrary :: Builder -> Library -> IO()
buildLibrary bld lib = do
    print "!!!"