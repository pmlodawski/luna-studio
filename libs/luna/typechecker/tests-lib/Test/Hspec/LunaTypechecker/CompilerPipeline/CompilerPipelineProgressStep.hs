module Test.Hspec.LunaTypechecker.CompilerPipeline.CompilerPipelineProgressStep where

data CompilerPipelineProgressStep = A_ParseStage1_AST
                                  | A_ParseStage1_ASTInfo
                                  | B_AnalysisStruct
                                  | C_ParseStage2_AST
                                  | C_ParseStage2_ASTInfo
                                  | D_DesugarImplicitSelf_AST
                                  | D_DesugarImplicitSelf_ASTInfo
                                  | E_Analysisstruct
                                  | F_TypecheckerInference_AST
                                  | F_TypecheckerInference_TCState
                                  | G_DesugarImplicitScopes_AST
                                  | G_DesugarImplicitScopes_ASTInfo
                                  | H_DesugarImplicitCalls_AST
                                  | H_DesugarImplicitCalls_ASTInfo
                                  | I_SSA
                                  | J_HsHASTGen
                                  | K_HsHSC
                                  | Pragmas