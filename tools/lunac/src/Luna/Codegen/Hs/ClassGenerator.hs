---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.ClassGenerator(
generateClass
) where

import qualified Luna.Network.Def.Definition     as Definition
import           Luna.Network.Def.Definition       (Definition)
import qualified Luna.Codegen.Hs.AST.DataType    as DataType
import           Luna.Codegen.Hs.AST.DataType      (DataType)
import qualified Luna.Codegen.Hs.AST.Expr        as Expr
import           Luna.Codegen.Hs.AST.Expr          (Expr(..))
import qualified Luna.Type.Type                  as Type
import qualified Luna.Codegen.Hs.Path            as Path
import qualified Luna.Codegen.Hs.AST.Module      as Module
import           Luna.Codegen.Hs.AST.Module        (Module)
import qualified Luna.Codegen.Hs.AST.Function    as Function
import qualified Luna.Codegen.Hs.Import          as Import
import qualified Luna.Codegen.Hs.AST.Deriving    as Deriving
import qualified Luna.Codegen.Hs.AST.Instance    as Instance


generateClass :: Definition -> Module -> (DataType, Module)
generateClass def m = (datatype, nmod) where

    --test     = Instance.empty { Instance.name   = "getter"
    --                          , Instance.params = [Expr.NTuple [dtcls]]
    --                          }


    cls        = Definition.cls def
    clsname    = Type.name cls
    params     = Type.params cls
    paramnames = map Type.name params
    fieldnames = map Path.mkFieldName paramnames
    paramtypes = map (Type.name . Type.cls) params
    fieldtypes = map Expr.Var fieldnames
    fields     = zipWith Expr.Typed fieldtypes paramtypes
    cons       = Expr.Cons clsname fields
    dtcls      = Expr.Type clsname (Type.typeparams cls)
    datatype   = DataType.addDeriving Deriving.Show
               $ DataType.empty { DataType.cls    = dtcls
                                , DataType.cons   = [cons]
                                }
    getters    = zipWith (Function.getter clsname) paramnames fieldnames
    setters    = zipWith (Function.setter clsname) paramnames fieldnames
    getnames   = map Path.mkGetter paramnames
    setnames   = map Path.mkSetter paramnames
    commimps   = map Import.common (getnames ++ setnames)
    nmod       = --Module.addInstance test
                 Module.addImports commimps
               $ Module.addExprs setters
               $ Module.addExprs getters
               $ Module.addDataType datatype
               $ m


