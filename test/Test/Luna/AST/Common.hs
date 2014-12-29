---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TupleSections #-}

module Test.Luna.Syntax.AST where

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.Prelude
import           Flowbox.Text.Show.Hs                       (hsShow)
import           Luna.AST.Module                            (Module)
import qualified Luna.Data.ASTInfo                          as ASTInfo
import qualified Luna.Data.Config                           as Config
import           Luna.Data.Namespace                        (Namespace (Namespace))
import           Luna.Data.Source                           (Source (Source))
import           Luna.Data.Source                           (Code (Code), File (File), Source (Source))
import qualified Luna.Data.Source                           as Source
import qualified Luna.Parser.Parser                         as Parser
import qualified Luna.Parser.Pragma                         as Pragma
import qualified Luna.Parser.State                          as State
import qualified Luna.Parser.State                          as ParserState
import qualified Luna.Pass                                  as Pass
import qualified Luna.Pass.Analysis.Alias.Alias             as Analysis.Alias
import qualified Luna.Pass.Analysis.Struct                  as SA
import qualified Luna.Pass.Target.HS.HASTGen                as HASTGen
import qualified Luna.Pass.Target.HS.HSC                    as HSC
import qualified Luna.Pass.Transform.Desugar.ImplicitCalls  as ImplCalls
import qualified Luna.Pass.Transform.Desugar.ImplicitScopes as ImplScopes
import qualified Luna.Pass.Transform.Desugar.ImplicitSelf   as ImplSelf
import qualified Luna.Pass.Transform.Hash                   as Hash
import qualified Luna.Pass.Transform.Parse.Stage1           as Stage1
import qualified Luna.Pass.Transform.Parse.Stage2           as Stage2
import qualified Luna.Pass.Transform.SSA                    as SSA
import           Luna.Syntax.Name                           (TName (TName))
import           System.Environment                         (getArgs)
import           System.IO                                  (stdout)
import           Text.Show.Pretty
--import Data.Default
--import Control.Monad.Trans.Either
--import Control.Monad.Trans.Class (lift)
--import qualified Data.ByteString as ByteStr
--import Data.Text.Lazy (unpack)



getAST :: String -> IO Module
getAST code = eitherStringToM' $ runEitherT $ do
    --(ast, _, astInfo) <- EitherT $ TxtParser.run $ Source ["Main"] code
    --(ast, astInfo)    <- EitherT $ Desugar.ImplicitSelf.run astInfo ast
    --(ast, astInfo)    <- EitherT $ Desugar.TLRecUpdt.run astInfo ast
    --aliasInfo         <- EitherT $ Analysis.Alias.run ast
    --callGraph         <- EitherT $ Analysis.CallGraph.run aliasInfo ast
    --ast               <- EitherT $ Transform.DepSort.run callGraph aliasInfo ast
    --(ast, astInfo)    <- EitherT $ Desugar.ImplicitScopes.run astInfo aliasInfo ast
    --(ast, _astInfo)   <- EitherT $ Desugar.ImplicitCalls.run astInfo ast
    --return ast

    let path = args !! 0
        src  = Source "Main" (File $ fromString path)

    (ast, astinfo) <- Pass.run1_ Stage1.pass src
    sa              <- Pass.run1_ SA.pass ast
    (ast, astinfo) <- Pass.run3_ Stage2.pass (Namespace [] sa) astinfo ast
    (ast, astinfo) <- Pass.run2_ ImplSelf.pass astinfo ast
    sa              <- Pass.run1_ SA.pass ast
    (ast, astinfo) <- Pass.run3_ ImplScopes.pass astinfo sa ast
    (ast, astinfo) <- Pass.run2_ ImplCalls.pass astinfo ast
    return ast
    --ast             <- Pass.run1_ SSA.pass ast
    --hast             <- Pass.run1_ HASTGen.pass ast
    --hsc              <- Pass.run1_ HSC.pass hast
