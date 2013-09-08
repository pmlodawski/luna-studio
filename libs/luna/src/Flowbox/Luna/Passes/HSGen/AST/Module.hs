---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.HSGen.AST.Module (
	module Flowbox.Luna.Passes.HSGen.AST.Module,
	module Flowbox.Luna.Passes.HSGen.AST.Expr
)where

import           Flowbox.Prelude
import           Flowbox.Luna.Passes.HSGen.AST.Expr


empty :: Expr 
empty = Module [] []


--import           Debug.Trace                             

--import           Data.Set                                (Set)
--import qualified Data.Set                              as Set

--import qualified Flowbox.Luna.Passes.HSGen.Path          as Path
--import           Flowbox.Luna.Passes.HSGen.Path            (Path)
--import qualified Flowbox.Luna.Passes.HSGen.Import        as Import
--import           Flowbox.Luna.Passes.HSGen.Import          (Import)
--import qualified Flowbox.Luna.Passes.HSGen.AST.Function  as Function
--import           Flowbox.Luna.Passes.HSGen.AST.Function    (Function)
--import qualified Flowbox.Luna.Passes.HSGen.AST.Instance  as Instance
--import           Flowbox.Luna.Passes.HSGen.AST.Instance    (Instance)
--import qualified Flowbox.Luna.Passes.HSGen.AST.DataType  as DataType
--import           Flowbox.Luna.Passes.HSGen.AST.DataType    (DataType)
--import qualified Flowbox.Luna.Passes.HSGen.AST.Class     as Class
--import           Flowbox.Luna.Passes.HSGen.AST.Class       (Class)
--import qualified Flowbox.Luna.Passes.HSGen.AST.Expr      as Expr
--import           Flowbox.Luna.Passes.HSGen.AST.Expr        (Expr)
--import qualified Flowbox.Luna.Passes.HSGen.AST.Extension as Extension
--import           Flowbox.Luna.Passes.HSGen.AST.Extension   (Extension)
--import           Data.String.Utils                       (join)

--data Module = Module { path       :: Path
--                     , submodules :: [Module]
--                     , imports    :: Set Import
--                     , datatypes  :: [DataType]
--                     , functions  :: [Function]
--                     , classes    :: [Class]
--                     , instances  :: [Instance]
--                     , exprs      :: [Expr]
--                     , extensions :: [Extension]
--                     } deriving (Show)

--empty :: Module
--empty = Module Path.empty [] Set.empty [] [] [] [] [] []

--base :: Module
--base = addImports [ Import.simple (Path.fromList ["Flowbox", "Luna", "Helpers", "Core"])
--                  , Import.simple (Path.fromList ["Flowbox", "Luna", "FClasses", "select0"])
--                  ]
--     $ empty

--header :: String
--header = "-- This is Flowbox generated file.\n\n"


--genCode :: Module -> String
--genCode m = header
--            ++ exts
--            ++ "module " ++ mypath ++ " where\n\n" 
--            ++ genSection "imports"     Import.genCode   (Set.elems $ imports m)
--            ++ genSection "datatypes"   DataType.genCode (datatypes m)
--            ++ genSection "functions"   Function.genCode (functions m)
--            ++ genSection "classes"     Class.genCode    (classes m)
--            ++ genSection "instances"   Instance.genCode (instances m)
--            ++ genSection "expressions" Expr.genCode     (exprs m)
--    where
--        exts   = Extension.genCode $ extensions m
--        mypath = (Path.toString . Path.toModulePath . path) m


--genSection :: String -> (a -> String) -> [a] -> String
--genSection header generator d = if null d 
--    then ""
--    else "-- " ++ header ++ "\n" ++ (join "\n" $ map generator d) ++ "\n\n"


--addExpr :: Expr -> Module -> Module
--addExpr expr self = self { exprs = expr : exprs self }


--addExprs :: [Expr] -> Module -> Module
--addExprs exprs' self = foldr addExpr self exprs'


--addAlias :: (String, String) -> Module -> Module
--addAlias alias = addExpr (Expr.mkAlias alias)


--addExt :: Extension -> Module -> Module
--addExt ext self = self {extensions = ext : extensions self}


--mkInst :: (String, String, String, String) -> Module -> Module
--mkInst (nameC, nameT, nameMT, name) = addExpr $ Expr.Call "mkInst''" (Expr.THTypeCtx nameC : map Expr.THExprCtx [nameT, nameMT, name]) Expr.Pure


--addDataType :: DataType -> Module -> Module
--addDataType dt self = self {datatypes = dt : datatypes self}


--addFunction :: Function -> Module -> Module
--addFunction func self = self {functions = func : functions self}


--addClass :: Class -> Module -> Module
--addClass cls self = self {classes = cls : classes self}


--addInstance :: Instance -> Module -> Module
--addInstance inst self = self {instances = inst : instances self}


--addImport :: Import -> Module -> Module
--addImport imp self = self {imports = Set.insert imp $ imports self}


--addImports :: [Import] -> Module -> Module
--addImports imps self = foldr addImport self imps