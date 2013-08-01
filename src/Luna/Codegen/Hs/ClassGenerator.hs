---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.ClassGenerator(
generateClass
) where

import qualified Luna.Network.Def.NodeDef        as NodeDef
import           Luna.Network.Def.NodeDef          (NodeDef)
import qualified Luna.Codegen.Hs.AST.DataType    as DataType
import qualified Luna.Codegen.Hs.AST.Cons        as Cons
import           Luna.Codegen.Hs.AST.Cons          (Cons(..))
import qualified Luna.Codegen.Hs.AST.Field       as Field
import           Luna.Codegen.Hs.AST.Field         (Field(..))
import qualified Luna.Type.Type                  as Type

generateClass def = datatype where
	cls        = NodeDef.cls def
	clsname    = Type.name cls
	params     = Type.params cls
	paramnames = map Type.name params
	paramtypes = map (Type.name . Type.cls) params
	fields     = zipWith Field paramnames paramtypes
	datatype   = DataType.empty { DataType.name       = clsname
								, DataType.typeparams = Type.typeparams cls
								, DataType.cons       = [Cons clsname fields]
								}

