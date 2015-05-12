---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Test.Luna.Syntax.AST where

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Luna.Data.ASTInfo                          (ASTInfo)
import           Luna.Data.Namespace                        (Namespace (Namespace))
import qualified Luna.Data.Namespace                        as Namespace
import           Luna.Data.Source                           (Source (Source), Text (Text))
import           Luna.Data.StructData                       (StructData (StructData))
import qualified Luna.Data.StructData                       as StructData
import qualified Luna.Parser.Parser                         as Parser
import qualified Luna.Pass                                  as Pass
import qualified Luna.Pass.Analysis.Imports                 as Imports
import qualified Luna.Pass.Analysis.Struct                  as SA
import qualified Luna.Pass.Transform.Desugar.ImplicitCalls  as ImplCalls
import qualified Luna.Pass.Transform.Desugar.ImplicitScopes as ImplScopes
import qualified Luna.Pass.Transform.Desugar.ImplicitSelf   as ImplSelf
import qualified Luna.Pass.Transform.Parse.Stage1           as Stage1
import qualified Luna.Pass.Transform.Parse.Stage2           as Stage2
import qualified Luna.Syntax.Enum                           as Enum
import           Luna.Syntax.Expr                           (LExpr)
import           Luna.Syntax.Module                         (LModule)
import           Luna.Syntax.Unit                           (Unit (Unit))
import           Luna.System.Session                        as Session



getAST :: String -> IO (LModule Enum.IDTag (LExpr Enum.IDTag ()), ASTInfo)
getAST code = fmap fst $ Session.runT $ do
    void Parser.init
    eitherStringToM' $ runEitherT $ do
        let src  = Source "Main" (Text $ fromString code)
        (ast, astInfo) <- Pass.run1_ Stage1.pass src
        importInfo     <- Pass.run1_ Imports.pass ast
        sd             <- Pass.run2_ SA.pass (StructData mempty importInfo) ast
        let structInfo = sd ^. StructData.namespace . Namespace.info
        (ast, astInfo) <- Pass.run3_ Stage2.pass (Namespace [] structInfo) astInfo ast
        (ast, astInfo) <- Pass.run2_ ImplSelf.pass astInfo ast
        sd             <- Pass.run2_ SA.pass sd ast
        let structInfo = sd ^. StructData.namespace . Namespace.info
            importInfo = sd ^. StructData.importInfo
        (ast, astInfo) <- Pass.run2_ ImplScopes.pass (astInfo, structInfo, importInfo) ast
        (ast, astInfo) <- Pass.run2_ ImplCalls.pass astInfo ast
        let Unit retAST = ast
        return (retAST, astInfo)
