{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Hspec.LunaTypechecker.CompilerPipeline.CompilerPipelineProgress where


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




data CompilerPipelineProgress 
    = CompilerPipelineProgress  { _a_parsestage1_ast                :: Maybe (Unit.Unit (Module.LModule Enum.IDTag String))
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

makeLenses ''CompilerPipelineProgress


instance Default CompilerPipelineProgress where
    def = CompilerPipelineProgress  { _a_parsestage1_ast                = Nothing
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
