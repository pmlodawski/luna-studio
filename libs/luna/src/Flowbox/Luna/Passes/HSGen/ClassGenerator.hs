---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.HSGen.ClassGenerator where

import           Flowbox.Prelude

--import qualified Flowbox.Luna.Network.Def.Definition  as Definition
--import           Flowbox.Luna.Network.Def.Definition    (Definition)
--import qualified Flowbox.Luna.Passes.HSGen.AST.DataType as DataType
--import           Flowbox.Luna.Passes.HSGen.AST.DataType   (DataType)
--import qualified Flowbox.Luna.Passes.HSGen.AST.Expr     as Expr
--import           Flowbox.Luna.Passes.HSGen.AST.Expr       (Expr(..))
--import qualified Flowbox.Luna.Type.Type               as Type
--import qualified Flowbox.Luna.Passes.HSGen.Path         as Path
--import qualified Flowbox.Luna.Passes.HSGen.AST.Module   as Module
--import           Flowbox.Luna.Passes.HSGen.AST.Module     (Module)
--import qualified Flowbox.Luna.Passes.HSGen.AST.Function as Function
--import qualified Flowbox.Luna.Passes.HSGen.Import       as Import
--import qualified Flowbox.Luna.Passes.HSGen.AST.Deriving as Deriving
--import qualified Flowbox.Luna.Passes.HSGen.AST.Instance as Instance


--generateClass :: Definition -> Module -> (DataType, Module)
--generateClass def m = (datatype, nmod) where

--    --test     = Instance.empty { Instance.name   = "getter"
--    --                          , Instance.params = [Expr.NTuple [dtcls]]
--    --                          }


--    cls        = Definition.cls def
--    clsname    = Type.name cls
--    params     = Type.params cls
--    paramnames = map Type.name params
--    fieldnames = map Path.mkFieldName paramnames
--    paramtypes = map (Expr.Var . Type.name . Type.cls) params
--    fieldtypes = map Expr.Var fieldnames
--    fields     = zipWith Expr.Typed fieldtypes paramtypes
--    cons       = Expr.Cons clsname fields
--    dtcls      = Expr.Type clsname (Type.typeparams cls)
--    datatype   = DataType.addDeriving Deriving.Show
--               $ DataType.empty { DataType.cls    = dtcls
--                                , DataType.cons   = [cons]
--                                }
--    getters    = map (Function.getter clsname) paramnames
--    setters    = map (Function.setter clsname) paramnames
--    getnames   = map Path.mkGetter paramnames
--    setnames   = map Path.mkSetter paramnames
--    commimps   = map Import.common (getnames ++ setnames)
--    nmod       = --Module.addInstance test
--                 Module.addImports commimps
--               $ Module.addExprs setters
--               $ Module.addExprs getters
--               $ Module.addDataType datatype
--               $ m


