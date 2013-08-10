---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Codegen.Hs.DefGenerator(
    generateDefinition,
    generateModule
) where

import           Debug.Trace                              


import           Data.List                                (zip4)
import           Data.Maybe                               (fromJust)

import qualified Flowbox.Luna.Network.Def.DefManager    as DefManager
import           Flowbox.Luna.Network.Def.DefManager      (DefManager)
import qualified Flowbox.Luna.Network.Path.Path         as Path
import qualified Flowbox.Luna.Network.Def.Definition    as Definition
import qualified Flowbox.Luna.Type.Type                 as Type
import qualified Flowbox.Luna.Codegen.Hs.Import         as Import
import qualified Flowbox.Luna.Data.Graph                as Graph

import qualified Flowbox.Luna.Codegen.Hs.AST.Module     as Module
import           Flowbox.Luna.Codegen.Hs.AST.Module       (Module)
import qualified Flowbox.Luna.Codegen.Hs.FuncGenerator  as FG
import qualified Flowbox.Luna.Codegen.Hs.ClassGenerator as CG
import qualified Flowbox.Luna.Codegen.Hs.Path           as Path
import qualified Flowbox.Luna.Codegen.Hs.AST.Function   as Function
import qualified Flowbox.Luna.Codegen.Hs.AST.Instance   as Instance
import qualified Flowbox.Luna.Codegen.Hs.AST.Class      as Class
import qualified Flowbox.Luna.Codegen.Hs.AST.Extension  as Extension
import qualified Flowbox.Luna.Codegen.Hs.AST.DataType   as DataType
import qualified Flowbox.Luna.Codegen.Hs.AST.Expr       as Expr

import           Flowbox.Luna.Data.List                   


generateDefinition :: DefManager -> Graph.Vertex -> Module
generateDefinition manager vtx = nmod where
    --d   = Graph.lab manager vtx
    def = fromJust $ Graph.lab manager vtx
    cls = Definition.cls def
    nmod = case cls of
        Type.Module   {} -> m where
            basemod  = generateModule manager vtx
            submods  = Module.submodules basemod
            subpaths = map Module.path submods
            subimps  = map Import.simple subpaths 
            m        = Module.addImports subimps basemod

        Type.Function {} -> m where
            basemod  = generateModule manager vtx
            submods  = Module.submodules basemod
            subpaths = map Module.path submods
            subimps  = map Import.qualified subpaths
            subnames = map Path.last subpaths
            subsrcs  = map (Path.toString . Path.toModulePath) subpaths
            aliases  = [(name, src ++ "." ++ name) | (name, src) <- zip subnames subsrcs]
            (basefunc, basemod2) = FG.generateFunction def basemod
            func     = foldr Function.addAlias basefunc aliases
            m        = Module.addFunction func
                     $ Module.addImports subimps
                     $ basemod2

        Type.Class {} -> m where
            basemod   = generateModule manager vtx
            submods   = Module.submodules basemod
            subpaths  = map Module.path submods
            subimps   = map Import.qualified subpaths
            subnames  = map Path.last subpaths
            subnamesM = map Path.mkMonadName subnames
            commimps  = map Import.common subnames

            csubnames   = map Path.mkClassName subnames
            subnamesT   = map Path.mkTemplateName subnames
            subnamesTM  = map Path.mkMonadName subnamesT
            modsubpaths = map Path.toModulePath subpaths
            impfuncs    = map Path.toString $ zipWith Path.append subnames  modsubpaths
            impfuncsM   = map Path.toString $ zipWith Path.append subnamesM modsubpaths

            funcs    = zipWith (Function.mkSpec (DataType.name dt)) subnamesT impfuncs
        

            (dt, modproto) = CG.generateClass def basemod
            m        = foldri Module.mkInst instargs
                     $ foldri Module.addFunction funcs
                     $ Module.addImports subimps 
                     $ Module.addImports commimps 
                     $ Module.addExt Extension.TemplateHaskell
                     $ Module.addExt Extension.FlexibleInstances
                     $ Module.addExt Extension.MultiParamTypeClasses
                     $ Module.addExt Extension.UndecidableInstances       --FIXME[wd]: Czy mozna sie tego pozbyc?
                     $ modproto

            instargs = zip4 csubnames subnamesT subnamesTM subnames

        _            -> error "Not known type conversion."


generateModule :: DefManager -> Graph.Vertex -> Module
generateModule manager vtx  = m where
    path        = Path.fromList $ DefManager.pathNames manager vtx 
    outnodes    = DefManager.suc manager vtx
    modules     = map (generateDefinition manager) outnodes
    m           = Module.base { Module.path       = path
                              , Module.submodules = modules
                              }


