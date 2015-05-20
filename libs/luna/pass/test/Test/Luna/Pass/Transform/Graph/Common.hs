---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Test.Luna.Pass.Transform.Graph.Common where

import           Flowbox.Control.Error
import           Flowbox.Prelude
import qualified Luna.Control.BCZipper                     as BCZipper
import           Luna.Control.Crumb                        (Breadcrumbs)
import qualified Luna.Control.Crumb                        as Crumb
import qualified Luna.Control.Focus                        as Focus
import           Luna.Data.ASTInfo                         (ASTInfo)
import qualified Luna.Data.Namespace                       as Namespace
import           Luna.Data.StructData                      (StructData (StructData))
import qualified Luna.Data.StructData                      as StructData
import qualified Luna.Pass                                 as Pass
import qualified Luna.Pass.Analysis.Imports                as Imports
import qualified Luna.Pass.Analysis.Struct                 as Struct
import qualified Luna.Pass.Transform.Graph.Builder.Builder as GraphBuilder
import qualified Luna.Pass.Transform.Graph.Parser.Parser   as GraphParser
import           Luna.Syntax.Decl                          (LDecl)
import           Luna.Syntax.Graph.Graph                   (Graph)
import           Luna.Syntax.Graph.Tag                     (Tag)
import           Luna.Syntax.Graph.Tag                     (TModule)
import           Luna.Syntax.Module                        (LModule)
import           Luna.Syntax.Unit                          (Unit (Unit))
import qualified Luna.System.Pragma.Store                  as Pragma
import           Luna.System.Session                       as Session



named :: a -> b -> (a, b)
named = (,)


mainBC :: Breadcrumbs
mainBC = [Crumb.Module "Main", Crumb.Function [] "main"]


type V = ()


getGraph :: Breadcrumbs -> TModule V -> IO (TModule V, Graph Tag V)
getGraph bc ast = runPass $ do
    zipper <- lift $ eitherStringToM $ BCZipper.focusBreadcrumbs' bc ast
    let focus = BCZipper.getFocus zipper
    decl  <- focus ^? Focus.decl <??> "test.Common.getFunctionGraph : Target is not a function"
    importInfo     <- Pass.run1_ Imports.pass (Unit ast)
    structData <- Pass.run2_ Struct.pass (StructData mempty importInfo) ast
    (decl2, graph) <- GraphBuilder.run (structData ^. StructData.namespace . Namespace.info) decl
    let newFocus = focus & Focus.decl .~ decl2
    return (BCZipper.close $ BCZipper.modify (const newFocus) zipper, graph)


getExpr :: Breadcrumbs -> Graph Tag V -> TModule V -> ASTInfo -> IO (TModule V, ASTInfo)
getExpr bc graph ast astInfo = runPass $ do
    zipper <- lift $ eitherStringToM $ BCZipper.focusBreadcrumbs' bc ast
    let focus = BCZipper.getFocus zipper
    decl <- focus ^? Focus.decl <??> "test.Common.getExpr : Target is not a function"
    (decl2, astInfo2) <- GraphParser.run graph decl astInfo
    let newFocus = focus & Focus.decl .~ decl2
    return (BCZipper.close $ BCZipper.modify (const newFocus) zipper, astInfo2)


getMain :: (Show a, Show e) => LModule a e -> IO (LDecl a e)
getMain ast = eitherStringToM' $ runEitherT $ do
    focus <- hoistEither $ BCZipper.getFocus <$> BCZipper.focusBreadcrumbs' mainBC ast
    Focus.getFunction focus <??> "test.Common.getMain : Target is not a function"


runPass :: (MonadIO m, Show a) => EitherT a (Pragma.PragmaStoreT m) b -> m b
runPass pass = do
    result <- Session.runT $ runEitherT pass
    eitherToM $ fst result

