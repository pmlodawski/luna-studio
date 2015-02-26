{-# LANGUAGE RecordWildCards #-}

module Test.Hspec.LunaTypechecker.CompilerPipeline.CompilerPipelineResult where


import            Flowbox.Prelude

import qualified  Luna.Pass                                                                 as Pass
import qualified  Luna.System.Pragma                                                        as Pragma

import qualified  Text.PrettyPrint                                                          as PP
import            Text.PrettyPrint                                                          (($+$))

import            Text.Show.Pretty                                                          (ppShow)

import            Luna.Typechecker.Debug.ConsoleColours
import            Luna.Typechecker.StageTypecheckerState.Class                              (prettyState)

import            Test.Hspec.LunaTypechecker.CompilerPipeline.CompilerPipelineProgress
import            Test.Hspec.LunaTypechecker.CompilerPipeline.CompilerPipelineProgressStep  (CompilerPipelineProgressStep(..))



newtype CompilerPipelineResult = CompilerPipelineResult { fromResult :: ((Either Pass.PassError (), CompilerPipelineProgress), Pragma.PragmaMap) }


pipelineRawResult :: Iso' CompilerPipelineResult ((Either Pass.PassError (), CompilerPipelineProgress), Pragma.PragmaMap)
pipelineRawResult = iso fromResult CompilerPipelineResult


instance Show CompilerPipelineResult where
    show = PP.render . prettyCompilerPipelineResult [D_DesugarImplicitSelf_AST, F_TypecheckerInference_TCState]



prettyCompilerPipelineResult :: [CompilerPipelineProgressStep] -> CompilerPipelineResult -> PP.Doc
prettyCompilerPipelineResult showSteps (CompilerPipelineResult ((eith, CompilerPipelineProgress{..}), pragmaMap)) =
    case eith of
        Left errMsg -> PP.text "Compilation failed."     $+$ errorResult errMsg
        Right ()    -> PP.text "Compilation successful." $+$ result
  where
    result = foldr ($+$) PP.empty (stepToDoc <$> showSteps)
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
                  $+$ body
                  $+$ PP.text ""
                  $+$ PP.text ""

    stepToDoc A_ParseStage1_AST               = stage1a
    stepToDoc A_ParseStage1_ASTInfo           = stage1i
    stepToDoc B_AnalysisStruct                = stage2
    stepToDoc C_ParseStage2_AST               = stage3a
    stepToDoc C_ParseStage2_ASTInfo           = stage3i
    stepToDoc D_DesugarImplicitSelf_AST       = stage4a
    stepToDoc D_DesugarImplicitSelf_ASTInfo   = stage4i
    stepToDoc E_Analysisstruct                = stage5
    stepToDoc F_TypecheckerInference_AST      = stage6a
    stepToDoc F_TypecheckerInference_TCState  = stage6i
    stepToDoc G_DesugarImplicitScopes_AST     = stage7a
    stepToDoc G_DesugarImplicitScopes_ASTInfo = stage7i
    stepToDoc H_DesugarImplicitCalls_AST      = stage8a
    stepToDoc H_DesugarImplicitCalls_ASTInfo  = stage8i
    stepToDoc I_SSA                           = stage9
    stepToDoc J_HsHASTGen                     = stage10
    stepToDoc K_HsHSC                         = stage11
    stepToDoc Pragmas                         = pragmas

    maybeShow :: (Show a) => Maybe a -> PP.Doc
    maybeShow = PP.text . maybe "╳" ppShow
    maybeText prettyFun = maybe PP.empty prettyFun

    splittr = PP.text $ [Yellow] `colouredFmt` "--------------------------------------------------------------------------------------------------------------"
    stage1  = stage1a $+$ stage1i
    stage3  = stage3a $+$ stage3i
    stage4  = stage4a $+$ stage4i
    stage6  = stage6a $+$ stage6i
    stage7  = stage7a $+$ stage7i
    stage8  = stage8a $+$ stage8i
    stage1a = section " 1. Stage1 :: AST"         (maybeShow              _a_parsestage1_ast              )
    stage1i = section " 1. Stage1 :: ASTInfo"     (maybeShow              _a_parsestage1_astinfo          )
    stage2  = section " 2. SA"                    (maybeShow              _b_analysisstruct               )
    stage3a = section " 3. Stage2 :: AST"         (maybeShow              _c_parsestage2_ast              )
    stage3i = section " 3. Stage2 :: ASTInfo"     (maybeShow              _c_parsestage2_astinfo          )
    stage4a = section " 4. ImplSelf :: AST"       (maybeShow              _d_desugarimplicitself_ast      )
    stage4i = section " 4. ImplSelf :: ASTInfo"   (maybeShow              _d_desugarimplicitself_astinfo  )
    stage5  = section " 5. SA"                    (maybeShow              _e_analysisstruct               )
    stage6a = section " 6. PTyChk :: AST"         (maybeShow              _f_typecheckerinference_ast     )
    stage6i = section " 6. PTyChk :: TCState"     (maybeText  prettyState _f_typecheckerinference_tcstate )
    stage7a = section " 7. ImplScopes :: AST"     (maybeShow              _g_desugarimplicitscopes_ast    )
    stage7i = section " 7. ImplScopes :: ASTInfo" (maybeShow              _g_desugarimplicitscopes_astinfo)
    stage8a = section " 8. ImplCalls :: AST"      (maybeShow              _h_desugarimplicitcalls_ast     )
    stage8i = section " 8. ImplCalls :: ASTInfo"  (maybeShow              _h_desugarimplicitcalls_astinfo )
    stage9  = section " 9. SSA"                   (maybeShow              _i_ssa                          )
    stage10 = section "10. HASTGen"               (maybeShow              _j_hshastgen                    )
    stage11 = section "11. HSC"                   (maybeShow              _k_hshsc                        )
    pragmas = section "    PragmaMap"             (maybeShow              (Just pragmaMap)                )
