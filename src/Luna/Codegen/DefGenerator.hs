---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.DefGenerator(
    generateDefinition
) where


import Debug.Trace

import qualified Luna.Codegen.ModGenerator       as MG
import qualified Luna.Codegen.FuncGenerator      as FG
import qualified Luna.Data.Graph                 as Graph
import qualified Luna.Type.Type                  as Type
import qualified Luna.Network.Def.NodeDef        as NodeDef
import           Luna.Network.Def.NodeDef          (NodeDef)
import qualified Luna.Network.Def.DefManager     as DefManager
import           Luna.Network.Def.DefManager       (DefManager)


generateDefinition :: Graph.Vertex -> DefManager -> String
generateDefinition vtx manager = code where
	def = Graph.lab_deprecated manager vtx
	cls = NodeDef.cls def
	code = case cls of
		Type.Function {} -> MG.generateModule vtx manager ++ FG.generateFunction def
		Type.Module   {} -> MG.generateModule vtx manager

