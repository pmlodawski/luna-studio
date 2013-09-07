---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.HSGen.CodeGenerator where

--import           Debug.Trace                              


--import           Data.List                                (zip4)

--import qualified Flowbox.Luna.Network.Def.DefManager    as DefManager
--import           Flowbox.Luna.Network.Def.DefManager      (DefManager)
--import qualified Flowbox.Luna.Network.Path.Path         as Path
--import qualified Flowbox.Luna.Network.Def.Definition    as Definition
--import qualified Flowbox.Luna.Type.Type                 as Type
--import qualified Flowbox.Luna.Passes.HSGen.Import         as Import
--import qualified Flowbox.Luna.Data.Graph                as Graph

--import qualified Flowbox.Luna.Passes.HSGen.AST.Module     as Module
--import           Flowbox.Luna.Passes.HSGen.AST.Module       (Module)
--import qualified Flowbox.Luna.Passes.HSGen.FuncGenerator  as FG
--import qualified Flowbox.Luna.Passes.HSGen.ClassGenerator as CG
--import qualified Flowbox.Luna.Passes.HSGen.DefGenerator   as DG
--import           Flowbox.Luna.Passes.HSGen.DefGenerator     (generateDefinition)
--import qualified Flowbox.Luna.Passes.HSGen.Path           as Path
--import qualified Flowbox.Luna.Passes.HSGen.AST.Function   as Function
--import qualified Flowbox.Luna.Passes.HSGen.AST.Instance   as Instance
--import qualified Flowbox.Luna.Passes.HSGen.AST.Class      as Class
--import qualified Flowbox.Luna.Passes.HSGen.AST.Extension  as Extension
--import qualified Flowbox.Luna.Passes.HSGen.AST.DataType   as DataType
--import qualified Flowbox.Luna.Passes.HSGen.AST.Expr       as Expr

--import           Flowbox.Luna.Data.List                   



--generateCommonCls :: String -> Module
--generateCommonCls name = m where

--    path       = Path.fromList ["Flowbox", "Common", name]
--    params     = [Expr.Type "a" [], Expr.Type "b" []]
--    paramsM    = [Expr.Type "a" [], Expr.Type "IO" ["b"]]
--    functype   = Expr.FuncType params
--    functypeM  = Expr.FuncType paramsM
--    nameM      = Path.mkMonadName name
--    cls        = Class.empty { Class.name   = Path.toModuleName name
--                             , Class.params = params
--                             , Class.deps   = [functype]
--                             , Class.fields = [ Expr.Typed (Expr.Var name)  functype
--                                              , Expr.Typed (Expr.Var nameM) functypeM
--                                              ]
--                             }
--    m           = Module.addExt Extension.FunctionalDependencies
--                $ Module.addExt Extension.FlexibleInstances
--                $ Module.addClass cls
--                $ Module.base { Module.path = path }



