{-# LANGUAGE TemplateHaskell #-}

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
import            Control.Monad.State.Strict

import            Data.Default
import            Data.Either
import qualified  Data.Text.Lazy                              as L

import            Test.Hspec.Expectations
import            Test.Hspec.LunaTypechecker.FileSystem       (strictReadFile)


type CompilerStepsRes = (Either Pass.PassError CompileSubSteps, Pragma.PragmaMap)


data CompileSubSteps 
    = CompileSubSteps { a_parsestage1            :: Maybe (Unit.Unit (Module.LModule Enum.IDTag String), ASTInfo.ASTInfo)
                      , b_analysisstruct         :: Maybe StructInfo.StructInfo
                      , c_parsestage2            :: Maybe (Unit.Unit (Module.LModule Enum.IDTag (Expr.LExpr Enum.IDTag ())), ASTInfo.ASTInfo)
                      , d_desugarimplicitself    :: Maybe (Unit.Unit (Module.LModule Enum.IDTag (Expr.LExpr Enum.IDTag ())), ASTInfo.ASTInfo)
                      , e_analysisstruct         :: Maybe StructInfo.StructInfo
                      , f_typecheckerinference   :: Maybe (Unit.Unit (Module.LModule Enum.IDTag (Expr.LExpr Enum.IDTag ())), Typechecker.StageTypecheckerState)
                      , g_desugarimplicitscopes  :: Maybe (Unit.Unit (Module.LModule Enum.IDTag (Expr.LExpr Enum.IDTag ())), ASTInfo.ASTInfo)
                      , h_desugarimplicitcalls   :: Maybe (Unit.Unit (Module.LModule Enum.IDTag (Expr.LExpr Enum.IDTag ())), ASTInfo.ASTInfo)
                      , i_ssa                    :: Maybe (Unit.Unit (Module.LModule Enum.IDTag (Expr.LExpr Enum.IDTag ())))
                      , j_hshastgen              :: Maybe HASTGen.HExpr
                      , k_hshsc                  :: Maybe L.Text
                      }
                      deriving (Show)

makeLenses ''CompileSubSteps


instance Default CompileSubSteps where
  def = CompileSubSteps { a_parsestage1           = Nothing
                        , b_analysisstruct        = Nothing
                        , c_parsestage2           = Nothing
                        , d_desugarimplicitself   = Nothing
                        , e_analysisstruct        = Nothing
                        , f_typecheckerinference  = Nothing
                        , g_desugarimplicitscopes = Nothing
                        , h_desugarimplicitcalls  = Nothing
                        , i_ssa                   = Nothing
                        , j_hshastgen             = Nothing
                        , k_hshsc                 = Nothing
                        }


lunaCompilerStepsFile :: String -> IO CompilerStepsRes
lunaCompilerStepsFile fileName =
    L.pack <$> strictReadFile fileName >>= lunaCompilerSteps (L.pack fileName)


lunaCompilerSteps :: L.Text -> L.Text -> IO CompilerStepsRes
lunaCompilerSteps fileName fileContents =
    Session.runT $ do
        Parser.init
        runEitherT $ do
            (ast1, astinfo1)     <- Pass.run1_ Stage1.pass src
            sa2                  <- Pass.run1_ SA.pass ast1
            (ast3, astinfo3)     <- Pass.run3_ Stage2.pass (Namespace [] sa2) astinfo1 ast1
            (ast4, astinfo4)     <- Pass.run2_ ImplSelf.pass astinfo3 ast3
            sa5                  <- Pass.run1_ SA.pass ast4
            (ast5, constraints)  <- Pass.run2_ PTyChk.tcpass ast4 sa5
            (ast6, astinfo6)     <- Pass.run3_ ImplScopes.pass astinfo4 sa5 ast5
            (ast7, astinfo7)     <- Pass.run2_ ImplCalls.pass astinfo6 ast6
            ast8                 <- Pass.run1_ SSA.pass ast7
            hast9                <- Pass.run1_ HASTGen.pass ast8
            hsc10                <- Pass.run1_ HSC.pass hast9
            return CompileSubSteps  { a_parsestage1           = Just $ (ast1, astinfo1)
                                    , b_analysisstruct        = Just $ sa2
                                    , c_parsestage2           = Just $ (ast3, astinfo3)
                                    , d_desugarimplicitself   = Just $ (ast4, astinfo4)
                                    , e_analysisstruct        = Just $ sa5
                                    , f_typecheckerinference  = Just $ (ast5, constraints)
                                    , g_desugarimplicitscopes = Just $ (ast6, astinfo6)
                                    , h_desugarimplicitcalls  = Just $ (ast7, astinfo7)
                                    , i_ssa                   = Just $ ast8
                                    , j_hshastgen             = Just $ hast9
                                    , k_hshsc                 = Just $ hsc10
                                    }
              
                
  where
    src = Source fileName (Text fileContents)


lunaCompilerStepsSuccess :: CompilerStepsRes -> Expectation
lunaCompilerStepsSuccess esuc = fst esuc `shouldSatisfy` isRight 
