{-# LANGUAGE RecordWildCards #-}

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

import Luna.Typechecker.Debug.ConsoleColours


import Test.Hspec.LunaTypechecker.CompilerPipeline.CompilerPipelineProgress


newtype CompilerPipelineResult = CompilerPipelineResult { fromResult :: ((Either Pass.PassError (), CompilerPipelineProgress), Pragma.PragmaMap) }

pipelineRawResult :: Iso' CompilerPipelineResult ((Either Pass.PassError (), CompilerPipelineProgress), Pragma.PragmaMap)
pipelineRawResult = iso fromResult CompilerPipelineResult

instance Show CompilerPipelineResult where
    show (CompilerPipelineResult ((eith, CompilerPipelineProgress{..}), pragmaMap)) =
        case eith of
            Left errMsg -> "Compilation failed.\n"     ++ PP.render (errorResult errMsg)
            Right ()    -> "Compilation successful.\n" ++ PP.render result
      where
        result  = PP.empty
             -- $+$ stage1
             -- $+$ stage2
             -- $+$ stage3
             -- $+$ stage4
              $+$ stage4a
              $+$ stage5
              $+$ stage6a
             -- $+$ stage6
             -- $+$ stage7
             -- $+$ stage8
             -- $+$ stage9
             -- $+$ stage10
             -- $+$ stage11
             -- $+$ pragmas
        errorResult errMsg =  splittr
                          $+$ PP.text "Error message"
                          $+$ splittr
                          $+$ PP.text errMsg
                          $+$ PP.text ""
                          $+$ PP.text ""
                          $+$ result
        section x body =  splittr
                      $+$ PP.text x
                      $+$ splittr
                      $+$ PP.text (maybe "╳" ppShow body)
                      $+$ PP.text ""
                      $+$ PP.text ""
        splittr = PP.text $ [Yellow] `colouredFmt` "--------------------------------------------------------------------------------------------------------------"
        stage1  = stage1a $+$ stage1i
        stage3  = stage3a $+$ stage3i
        stage4  = stage4a $+$ stage4i
        stage6  = stage6a $+$ stage6i
        stage7  = stage7a $+$ stage7i
        stage8  = stage8a $+$ stage8i
        stage1a = section " 1. Stage1 :: AST"         _a_parsestage1_ast
        stage1i = section " 1. Stage1 :: ASTInfo"     _a_parsestage1_astinfo
        stage2  = section " 2. SA"                    _b_analysisstruct
        stage3a = section " 3. Stage2 :: AST"         _c_parsestage2_ast
        stage3i = section " 3. Stage2 :: ASTInfo"     _c_parsestage2_astinfo
        stage4a = section " 4. ImplSelf :: AST"       _d_desugarimplicitself_ast
        stage4i = section " 4. ImplSelf :: ASTInfo"   _d_desugarimplicitself_astinfo
        stage5  = section " 5. SA"                    _e_analysisstruct
        stage6a = section " 6. PTyChk :: AST"         _f_typecheckerinference_ast
        stage6i = section " 6. PTyChk :: ASTInfo"     _f_typecheckerinference_astinfo
        stage7a = section " 7. ImplScopes :: AST"     _g_desugarimplicitscopes_ast
        stage7i = section " 7. ImplScopes :: ASTInfo" _g_desugarimplicitscopes_astinfo
        stage8a = section " 8. ImplCalls :: AST"      _h_desugarimplicitcalls_ast
        stage8i = section " 8. ImplCalls :: ASTInfo"  _h_desugarimplicitcalls_astinfo
        stage9  = section " 9. SSA"                   _i_ssa
        stage10 = section "10. HASTGen"               _j_hshastgen
        stage11 = section "11. HSC"                   _k_hshsc
        pragmas = section "    PragmaMap"             (Just pragmaMap)
