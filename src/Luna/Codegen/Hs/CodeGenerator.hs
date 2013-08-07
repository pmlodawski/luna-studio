---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.CodeGenerator(
    generateDefinition,
    generateCommonCls
) where

import Debug.Trace


import           Data.List                         (zip4)

import qualified Luna.Network.Def.DefManager     as DefManager
import           Luna.Network.Def.DefManager       (DefManager)
import qualified Luna.Network.Path.Path          as Path
import qualified Luna.Network.Def.Definition     as Definition
import qualified Luna.Type.Type                  as Type
import qualified Luna.Codegen.Hs.Import          as Import
import qualified Luna.Data.Graph                 as Graph

import qualified Luna.Codegen.Hs.AST.Module      as Module
import           Luna.Codegen.Hs.AST.Module        (Module)
import qualified Luna.Codegen.Hs.FuncGenerator   as FG
import qualified Luna.Codegen.Hs.ClassGenerator  as CG
import qualified Luna.Codegen.Hs.DefGenerator    as DG
import           Luna.Codegen.Hs.DefGenerator       (generateDefinition)
import qualified Luna.Codegen.Hs.Path            as Path
import qualified Luna.Codegen.Hs.AST.Function    as Function
import qualified Luna.Codegen.Hs.AST.Instance    as Instance
import qualified Luna.Codegen.Hs.AST.Class       as Class
import qualified Luna.Codegen.Hs.AST.Extension   as Extension
import qualified Luna.Codegen.Hs.AST.DataType    as DataType
import qualified Luna.Codegen.Hs.AST.Expr        as Expr

import           Luna.Data.List








generateCommonCls :: String -> Module
generateCommonCls name = m where

    path       = Path.fromList ["Flowbox", "Common", name]
    params     = [Expr.Type "a" [], Expr.Type "b" []]
    paramsM    = [Expr.Type "a" [], Expr.Type "IO" ["b"]]
    functype   = Expr.FuncType params
    functypeM  = Expr.FuncType paramsM
    nameM      = Path.mkMonadName name
    cls        = Class.empty { Class.name   = Path.toModuleName name
                             , Class.params = params
                             , Class.deps   = [functype]
                             , Class.fields = [ Expr.Typed (Expr.Var name)  functype
                                              , Expr.Typed (Expr.Var nameM) functypeM
                                              ]
                             }
    m           = Module.addExt Extension.FunctionalDependencies
                $ Module.addExt Extension.FlexibleInstances
                $ Module.addClass cls
                $ Module.base { Module.path = path }

    --outnames    = fmap (Type.name . NodeDef.cls) outnodes
    --outpaths    = fmap ((Path.add path) . Path.single) outnames
    --imports     = zipWith Import.single outpaths outnames
    --importstxt  = join "\n" $ fmap Import.genCode imports
    --header      = "module " ++ Path.toModulePath path
    --out         = header ++ MG.generateModuleReturn ++ importstxt ++ "\n\n"


