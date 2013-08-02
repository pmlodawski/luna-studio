---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.ClassGenerator(
generateClass
) where

import Debug.Trace


import qualified Luna.Network.Def.NodeDef        as NodeDef
import           Luna.Network.Def.NodeDef          (NodeDef)
import qualified Luna.Codegen.Hs.AST.DataType    as DataType
import qualified Luna.Codegen.Hs.AST.Cons        as Cons
import           Luna.Codegen.Hs.AST.Cons          (Cons(..))
import qualified Luna.Codegen.Hs.AST.Field       as Field
import           Luna.Codegen.Hs.AST.Field         (Field(..))
import qualified Luna.Type.Type                  as Type
import qualified Luna.Codegen.Hs.Path            as Path
import qualified Luna.Codegen.Hs.AST.Module      as Module
import           Luna.Codegen.Hs.AST.Module        (Module)
import qualified Luna.Codegen.Hs.AST.Expr        as Expr
import qualified Luna.Codegen.Hs.AST.Function    as Function

generateClass :: NodeDef -> Module -> Module
generateClass def mod = nmod where
    cls        = NodeDef.cls def
    clsname    = Type.name cls
    params     = Type.params cls
    paramnames = map Type.name params
    fieldnames = map Path.mkFieldName paramnames
    paramtypes = map (Type.name . Type.cls) params
    fields     = zipWith Field fieldnames paramtypes
    datatype   = DataType.empty { DataType.name       = clsname
                                , DataType.typeparams = Type.typeparams cls
                                , DataType.cons       = [Cons clsname fields]
                                }
    getters    = zipWith (Function.getter clsname) paramnames fieldnames
    setters    = zipWith (Function.setter clsname) paramnames fieldnames
    gettersM   = zipWith (Function.getterM clsname) paramnames fieldnames
    settersM   = zipWith (Function.setterM clsname) paramnames fieldnames
    nmod       = Module.addExprs settersM
               $ Module.addExprs gettersM
               $ Module.addExprs setters
               $ Module.addExprs getters
               $ Module.addDataType datatype
               $ mod


