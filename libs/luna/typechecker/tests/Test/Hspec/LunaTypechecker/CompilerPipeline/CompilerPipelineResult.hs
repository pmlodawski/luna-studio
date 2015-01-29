{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.LunaTypechecker.CompilerPipeline.CompilerPipelineResult where

import            Luna.Data.Namespace                         (Namespace(Namespace))
import            Luna.Data.Source                            (Text(Text), Source(Source))
import qualified  Luna.Parser.Parser                          as Parser
import qualified  Luna.System.Pragma                          as Pragma
import qualified  Luna.Pass                                   as Pass
import qualified  Luna.System.Session                         as Session

import qualified  Luna.Pass.Analysis.Struct                   as SA
import qualified  Luna.Pass.Target.HS.HASTGen                 as HASTGen
import qualified  Luna.Pass.Target.HS.HSC                     as HSC
import qualified  Luna.Pass.Transform.Desugar.ImplicitCalls   as ImplCalls
import qualified  Luna.Pass.Transform.Desugar.ImplicitScopes  as ImplScopes
import qualified  Luna.Pass.Transform.Desugar.ImplicitSelf    as ImplSelf
import qualified  Luna.Pass.Transform.Parse.Stage1            as Stage1
import qualified  Luna.Pass.Transform.Parse.Stage2            as Stage2
import qualified  Luna.Pass.Transform.SSA                     as SSA
import qualified  Luna.Typechecker.Inference                  as PTyChk

import qualified  Luna.Data.ASTInfo                           as ASTInfo
import qualified  Luna.Data.StructInfo                        as StructInfo
import qualified  Luna.Syntax.Enum                            as Enum
import qualified  Luna.Syntax.Expr                            as Expr
import qualified  Luna.Syntax.Module                          as Module
import qualified  Luna.Syntax.Unit                            as Unit
import qualified  Luna.Typechecker.StageTypecheckerState      as Typechecker

import            Control.Applicative
import            Control.Lens
import            Control.Monad.Trans.Either
import            Control.Monad.Trans.Class
import            Control.Monad.State.Strict
import qualified  Luna.System.Pragma.Store as Store

import            Data.Default
import            Data.Either
import            Data.Maybe
import qualified  Data.Text.Lazy                              as L

import            Test.Hspec.Expectations.LunaTypechecker
import            Test.Hspec.LunaTypechecker.FileSystem       (strictReadFile)

import qualified  Text.PrettyPrint                            as PP
import            Text.PrettyPrint                            (($+$), text, empty)
import            Text.Show.Pretty                            (ppShow)


import Test.Hspec.LunaTypechecker.CompilerPipeline.CompilerPipelineProgress


newtype CompilerPipelineResult = CompilerPipelineResult { fromResult :: ((Either Pass.PassError (), CompilerPipelineProgress), Pragma.PragmaMap) }

pipelineRawResult :: Iso' CompilerPipelineResult ((Either Pass.PassError (), CompilerPipelineProgress), Pragma.PragmaMap)
pipelineRawResult = iso fromResult CompilerPipelineResult

instance Show CompilerPipelineResult where
    show _ = "<Show CompilerPipelineResult> to be implemented"

--newtype CompilerStepsResWrapper = CompilerStepsResWrapper { unWrapCompilerStepsRes :: CompilerStepsRes }

--instance Show CompilerStepsResWrapper where
--    show (CompilerStepsResWrapper (Left  errorMsg,            pragma)) = "Compiler failed!\n" ++ errorMsg
--    show (CompilerStepsResWrapper (Right CompilerPipelineProgress{..}, pragma)) = "Compilation successful\n" ++ show passes
--      where
--        passes =  text "--------------------------------------------------------------------------------"
--              $+$ text "A: parsestage1"
--              $+$ text "--------------------------------------------------------------------------------" 
--              $+$ text (maybe "???" ppShow _a_parsestage1)
--              $+$ PP.text " "

--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text "B: analysisstruct"
--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text (maybe "???" ppShow _b_analysisstruct)
--              $+$ PP.text " "

--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text "C: parsestage2"
--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text (maybe "???" ppShow _c_parsestage2)
--              $+$ PP.text " "

--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text "D: desugarimplicitself"
--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text (maybe "???" ppShow _d_desugarimplicitself)
--              $+$ PP.text " "

--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text "E: analysisstruct"
--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text (maybe "???" ppShow _e_analysisstruct)
--              $+$ PP.text " "

--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text "F: typecheckerinference"
--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text (maybe "???" (ppShow . snd)
--                       (_f_typecheckerinference :: Maybe (Unit.Unit (Module.LModule Enum.IDTag (Expr.LExpr Enum.IDTag ())), Typechecker.StageTypecheckerState)))
--              $+$ PP.text " "

--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text "G: desugarimplicitscopes"
--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text (maybe "???" ppShow _g_desugarimplicitscopes)
--              $+$ PP.text " "

--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text "H: desugarimplicitcalls"
--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text (maybe "???" ppShow _h_desugarimplicitcalls)
--              $+$ PP.text " "

--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text "I: ssa"
--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text (maybe "???" ppShow _i_ssa)
--              $+$ PP.text " "

--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text "J: hshastgen"
--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text (maybe "???" ppShow _j_hshastgen)
--              $+$ PP.text " "

--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text "K: hshsc"
--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text (L.unpack $ fromMaybe "???" _k_hshsc)
--              $+$ PP.text " "

--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text "pragmas"
--              $+$ text "--------------------------------------------------------------------------------"
--              $+$ text (ppShow pragma)