---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Test.Luna.Syntax.Common where

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Luna.Data.Namespace                        (Namespace (Namespace))
import           Luna.Data.Source                           (Source (Source), Text (Text))
import qualified Luna.Parser.Parser                         as Parser
import qualified Luna.Pass                                  as Pass
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
import           Luna.System.Pragma                         (PragmaMap)
import           Luna.System.Session                        as Session



getAST :: String -> IO (LModule Enum.IDTag (LExpr Enum.IDTag ()), PragmaMap)
getAST code =  Session.runT $ do
    void Parser.init
    eitherStringToM' $ runEitherT $ do
        let src  = Source "Main" (Text $ fromString code)
        (ast,  astinfo) <- Pass.run1_ Stage1.pass src
        sa              <- Pass.run1_ SA.pass ast
        (ast,  astinfo) <- Pass.run3_ Stage2.pass (Namespace [] sa) astinfo ast
        (ast,  astinfo) <- Pass.run2_ ImplSelf.pass astinfo ast
        sa              <- Pass.run1_ SA.pass ast
        (ast,  astinfo) <- Pass.run3_ ImplScopes.pass astinfo sa ast
        (ast, _astinfo) <- Pass.run2_ ImplCalls.pass astinfo ast
        let Unit retAST = ast
        return retAST
