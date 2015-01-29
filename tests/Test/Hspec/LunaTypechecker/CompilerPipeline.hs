{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.LunaTypechecker.CompilerPipeline where


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

import            Test.Hspec.Expectations
import            Test.Hspec.LunaTypechecker.FileSystem       (strictReadFile)

import qualified  Text.PrettyPrint                            as PP
import            Text.PrettyPrint                            (($+$), text, empty)
import            Text.Show.Pretty                            (ppShow)


type CompilerStepsRes = (Either Pass.PassError CompilePipelineProgress, Pragma.PragmaMap)


data CompilePipelineProgress 
    = CompilePipelineProgress { _a_parsestage1_ast                :: Maybe (Unit.Unit (Module.LModule Enum.IDTag String))
                              , _a_parsestage1_astinfo            :: Maybe ASTInfo.ASTInfo
                              , _b_analysisstruct                 :: Maybe StructInfo.StructInfo
                              , _c_parsestage2_ast                :: Maybe (Unit.Unit (Module.LModule Enum.IDTag (Expr.LExpr Enum.IDTag ())))
                              , _c_parsestage2_astinfo            :: Maybe ASTInfo.ASTInfo
                              , _d_desugarimplicitself_ast        :: Maybe (Unit.Unit (Module.LModule Enum.IDTag (Expr.LExpr Enum.IDTag ())))
                              , _d_desugarimplicitself_astinfo    :: Maybe ASTInfo.ASTInfo
                              , _e_analysisstruct                 :: Maybe StructInfo.StructInfo
                              , _f_typecheckerinference_ast       :: Maybe (Unit.Unit (Module.LModule Enum.IDTag (Expr.LExpr Enum.IDTag ())))
                              , _f_typecheckerinference_astinfo   :: Maybe Typechecker.StageTypecheckerState
                              , _g_desugarimplicitscopes_ast      :: Maybe (Unit.Unit (Module.LModule Enum.IDTag (Expr.LExpr Enum.IDTag ())))
                              , _g_desugarimplicitscopes_astinfo  :: Maybe ASTInfo.ASTInfo
                              , _h_desugarimplicitcalls_ast       :: Maybe (Unit.Unit (Module.LModule Enum.IDTag (Expr.LExpr Enum.IDTag ())))
                              , _h_desugarimplicitcalls_astinfo   :: Maybe ASTInfo.ASTInfo
                              , _i_ssa                            :: Maybe (Unit.Unit (Module.LModule Enum.IDTag (Expr.LExpr Enum.IDTag ())))
                              , _j_hshastgen                      :: Maybe HASTGen.HExpr
                              , _k_hshsc                          :: Maybe L.Text
                              }
                              deriving (Show)

makeLenses ''CompilePipelineProgress


instance Default CompilePipelineProgress where
    def = CompilePipelineProgress { _a_parsestage1_ast                = Nothing
                                  , _a_parsestage1_astinfo            = Nothing
                                  , _b_analysisstruct                 = Nothing
                                  , _c_parsestage2_ast                = Nothing
                                  , _c_parsestage2_astinfo            = Nothing
                                  , _d_desugarimplicitself_ast        = Nothing
                                  , _d_desugarimplicitself_astinfo    = Nothing
                                  , _e_analysisstruct                 = Nothing
                                  , _f_typecheckerinference_ast       = Nothing
                                  , _f_typecheckerinference_astinfo   = Nothing
                                  , _g_desugarimplicitscopes_ast      = Nothing
                                  , _g_desugarimplicitscopes_astinfo  = Nothing
                                  , _h_desugarimplicitcalls_ast       = Nothing
                                  , _h_desugarimplicitcalls_astinfo   = Nothing
                                  , _i_ssa                            = Nothing
                                  , _j_hshastgen                      = Nothing
                                  , _k_hshsc                          = Nothing
                                  }

newtype CompilerPipelineResult = CompilerPipelineResult { fromResult :: ((Either Pass.PassError (), CompilePipelineProgress),Pragma.PragmaMap) }

--newtype CompilerStepsResWrapper = CompilerStepsResWrapper { unWrapCompilerStepsRes :: CompilerStepsRes }

--instance Show CompilerStepsResWrapper where
--    show (CompilerStepsResWrapper (Left  errorMsg,            pragma)) = "Compiler failed!\n" ++ errorMsg
--    show (CompilerStepsResWrapper (Right CompilePipelineProgress{..}, pragma)) = "Compilation successful\n" ++ show passes
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


lunaCompilerStepsFile :: String -> IO ((Either Pass.PassError (), CompilePipelineProgress),Pragma.PragmaMap)
lunaCompilerStepsFile fileName = do
    res <- L.pack <$> strictReadFile fileName >>= lunaCompilerSteps (L.pack fileName)
    return res


lunaCompilerSteps :: L.Text -> L.Text -> IO ((Either Pass.PassError (), CompilePipelineProgress),Pragma.PragmaMap)
lunaCompilerSteps fileName fileContents = do
    res <- Session.runT $ do
        Parser.init
        runStateT (runEitherT procedure) (def :: CompilePipelineProgress)
    return $  res

  where
    src = Source fileName (Text fileContents)
    --procedure :: StateT CompilePipelineProgress (EitherT Pass.PassError (Store.PragmaStoreT IO)) ()
    procedure :: EitherT Pass.PassError (StateT CompilePipelineProgress (Store.PragmaStoreT IO)) ()
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
        f_typecheckerinference_astinfo  .= Just constraints

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


--lunaCompilerStepsSuccess :: CompilerStepsResWrapper -> Expectation
--lunaCompilerStepsSuccess all@(CompilerStepsResWrapper esuc) = do
--  all `shouldSatisfy` (isRight . fst . unWrapCompilerStepsRes)


--lunaCompilerStepsFailure :: CompilerStepsResWrapper -> Expectation
--lunaCompilerStepsFailure all@(CompilerStepsResWrapper esuc) = do
--  all `shouldSatisfy` (isLeft . fst . unWrapCompilerStepsRes)

--lunaCompilerStepsSuccess :: CompilerStepsRes -> Expectation
--lunaCompilerStepsSuccess all = do
--  all `shouldSatisfy` (isRight . fst)


--lunaCompilerStepsFailure :: CompilerStepsRes -> Expectation
--lunaCompilerStepsFailure all = do
--  all `shouldSatisfy` (isLeft . fst)



lunaCompilerStepsSuccess :: (Either Pass.PassError (), CompilePipelineProgress) -> Expectation
lunaCompilerStepsSuccess _ =
  1 `shouldBe` (1 :: Int)

lunaCompilerStepsFailure :: (Either Pass.PassError (), CompilePipelineProgress) -> Expectation
lunaCompilerStepsFailure _ =
  1 `shouldBe` (1 :: Int)

