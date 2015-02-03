module Test.Hspec.LunaTypechecker.CompilerPipeline (
    module CompilerPipelineResult,
    module CompilerPipelineProgress,
    lunacStepsSuccess, lunacStepsFailure,
    lunacStepsSuccessFile, lunacStepsFailureFile,
    lunacStepsFile, lunacSteps
  ) where


import            Flowbox.Prelude

import            Test.Hspec.LunaTypechecker.CompilerPipeline.CompilerPipelineResult   as CompilerPipelineResult
import            Test.Hspec.LunaTypechecker.CompilerPipeline.CompilerPipelineProgress as CompilerPipelineProgress

import            Control.Monad.Trans.Either
import            Control.Monad.State.Strict

import qualified  Data.Text.Lazy                              as L

import            Test.Hspec.Expectations.LunaTypechecker
import            Test.Hspec.LunaTypechecker.FileSystem       (strictReadFile)

import            Luna.Data.Namespace                         (Namespace(Namespace))
import qualified  Luna.Data.Source                            as Src

import qualified  Luna.Parser.Parser                          as Parser

import qualified  Luna.Pass                                   as Pass
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

import qualified  Luna.System.Session                         as Session


lunacStepsSuccessFile :: String -> Expectation
lunacStepsSuccessFile fileName = lunacStepsSuccess =<< lunacStepsFile fileName

lunacStepsFailureFile :: String -> Expectation
lunacStepsFailureFile fileName = lunacStepsFailure =<< lunacStepsFile fileName


lunacStepsSuccess :: CompilerPipelineResult -> Expectation
lunacStepsSuccess res = res `shouldSatisfyLens` (pipelineRawResult . _1 . _1 . to isRight)


lunacStepsFailure :: CompilerPipelineResult -> Expectation
lunacStepsFailure res = res `shouldSatisfyLens` (pipelineRawResult . _1 . _1 . to isLeft)


lunacStepsFile :: String -> IO CompilerPipelineResult
lunacStepsFile fileName = L.pack <$> strictReadFile fileName >>= lunacSteps (L.pack fileName)


lunacSteps :: Text -> Text -> IO CompilerPipelineResult
lunacSteps fileName fileContents = do
    res <- Session.runT $ do  Parser.init
                              runStateT (runEitherT procedure) (def :: CompilerPipelineProgress)
    return $ CompilerPipelineResult res

  where
    src = Src.Source fileName (Src.Text fileContents)
    procedure = do
        (ast1, astinfo1)                <- Pass.run1_ Stage1.pass src
        a_parsestage1_ast               .= Just ast1
        a_parsestage1_astinfo           .= Just astinfo1

        sa2                             <- Pass.run1_ SA.pass ast1
        b_analysisstruct                .= Just sa2 

        (ast3, astinfo3)                <- Pass.run3_ Stage2.pass (Namespace [] sa2) astinfo1 ast1
        c_parsestage2_ast               .= Just ast3
        c_parsestage2_astinfo           .= Just astinfo3

        (ast4, astinfo4)                <- Pass.run2_ ImplSelf.pass astinfo3 ast3
        d_desugarimplicitself_ast       .= Just ast4
        d_desugarimplicitself_astinfo   .= Just astinfo4

        sa5                             <- Pass.run1_ SA.pass ast4
        e_analysisstruct                .= Just sa5 

        (ast5, constraints)             <- Pass.run2_ PTyChk.tcpass ast4 sa5
        f_typecheckerinference_ast      .= Just ast5
        f_typecheckerinference_tcstate  .= Just constraints

        (ast6, astinfo6)                <- Pass.run3_ ImplScopes.pass astinfo4 sa5 ast5
        g_desugarimplicitscopes_ast     .= Just ast6
        g_desugarimplicitscopes_astinfo .= Just astinfo6

        (ast7, astinfo7)                <- Pass.run2_ ImplCalls.pass astinfo6 ast6
        h_desugarimplicitcalls_ast      .= Just ast7
        h_desugarimplicitcalls_astinfo  .= Just astinfo7

        ast8                            <- Pass.run1_ SSA.pass ast7
        i_ssa                           .= Just ast8 

        hast9                           <- Pass.run1_ HASTGen.pass ast8
        j_hshastgen                     .= Just hast9 

        hsc10                           <- Pass.run1_ HSC.pass hast9
        k_hshsc                         .= Just hsc10 

        return ()

