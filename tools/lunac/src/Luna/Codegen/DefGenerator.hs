---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.DefGenerator(
    generateModule
) where


import Debug.Trace

import           Data.String.Utils                 (join)

import qualified Data.Graph.Inductive            as DG
import qualified Luna.Network.Def.DefManager     as DefManager
import           Luna.Network.Def.DefManager       (DefManager)
import qualified Luna.Network.Path.Path          as Path
import qualified Luna.Network.Def.NodeDef        as NodeDef
import           Luna.Network.Def.NodeDef          (NodeDef)
import qualified Luna.Network.Def.Edge           as Edge
import           Luna.Network.Def.Edge             (Edge(..))
import qualified Luna.Type.Type                  as Type
import qualified Luna.Network.Path.Import        as Import

generateModule :: DG.Node -> DefManager -> String
generateModule = generateModuleCode

generateModuleCode :: DG.Node -> DefManager -> String
generateModuleCode did manager = out where
    path        = DefManager.pathOf did manager
    header      = "module " ++ Path.toModulePath path
    edges       = DefManager.out manager did
    outnums     = [dst | Edge src dst <- edges]
    outnodes    = fmap (DefManager.nodeById manager) $ outnums
    outnames    = fmap (Type.name . NodeDef.cls) outnodes
    outpaths    = fmap ((Path.add path) . Path.single) outnames
    imports     = zipWith Import.single outpaths outnames
    importstxt  = join "\n" $ fmap Import.genCode imports
    out         = header ++ generateModuleReturn ++ importstxt



generateModuleReturn :: String
generateModuleReturn = "where\n"