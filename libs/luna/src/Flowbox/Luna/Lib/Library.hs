---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Lib.Library(
    Library(..),
    ID,
    empty,
    make,
    store,
    storeDef
) where

import qualified Flowbox.System.UniPath               as UniPath
import           Flowbox.System.UniPath                 (UniPath)
import qualified Flowbox.Luna.Network.Def.DefManager  as DefManager
import           Flowbox.Luna.Network.Def.DefManager    (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition  as Definition
import           Flowbox.Luna.Network.Def.Definition    (Definition)
import qualified Flowbox.Luna.Codegen.Hs.DefGenerator as DG
import qualified Flowbox.Luna.Codegen.Hs.AST.Module   as Module
import qualified Flowbox.Luna.Codegen.Hs.Path         as Path
import qualified Flowbox.System.IO                    as IO
import           Flowbox.System.Directory             as Dir



data Library =  Library{ name :: String
                       , path :: UniPath
                       , defs :: DefManager
                       } deriving (Show)

type ID  = Int


empty :: Library
empty = Library "" UniPath.empty DefManager.empty


make :: String -> UniPath -> Library
make name' path' = empty { name = name'
                         , path = path'
                         , defs = DefManager.insNode (0, rootdef) DefManager.empty
                         } where
    rootdef = Definition.mkModule name'


store :: Library -> IO ()
store lib = do 
    let
        nodes = DefManager.nodes $ defs lib
    mapM (storeDef lib) nodes
    return ()

storeDef :: Library -> Definition.ID -> IO ()
storeDef lib did = do
    let mod  = DG.generateDefinition (defs lib) did
        code = Module.genCode mod
        modpath = Path.toFilePath $ Module.path mod
        modupath = UniPath.fromList $ Path.segments modpath
        path' = (path lib) ++ modupath

    Dir.createDirectoryIfMissing True $ UniPath.dirOf path'
    IO.writeFile path' code
    return ()