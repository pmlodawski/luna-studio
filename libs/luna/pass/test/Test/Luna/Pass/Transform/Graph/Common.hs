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
import qualified Luna.Pass                                 as Pass
import qualified Luna.Pass.Analysis.Struct                 as Struct
import qualified Luna.Pass.Transform.Graph.Builder.Builder as GraphBuilder
import qualified Luna.Pass.Transform.Graph.Parser.Parser   as GraphParser
import qualified Luna.Syntax.Control.BCZipper              as BCZipper
import           Luna.Syntax.Control.Crumb                 (Breadcrumbs)
import qualified Luna.Syntax.Control.Crumb                 as Crumb
import qualified Luna.Syntax.Control.Focus                 as Focus
import           Luna.Syntax.Decl                          (LDecl)
import           Luna.Syntax.Enum                          (Enumerated)
import           Luna.Syntax.Graph.Graph                   (Graph)
import           Luna.Syntax.Graph.Tag                     (TDecl, TModule)
import           Luna.Syntax.Module                        (LModule)
import           Luna.System.Session                       as Session


named :: a -> b -> (a, b)
named = (,)


mainBC :: Breadcrumbs
mainBC = [Crumb.Module "Main", Crumb.Function [] "main"]


type V = ()


getGraph :: Enumerated a => Breadcrumbs -> TModule V -> IO (TDecl V, Graph a V)
getGraph bc ast = runPass $ do
    focus <- lift $ eitherStringToM $ BCZipper.getFocus <$> BCZipper.focusBreadcrumbs' bc ast
    decl  <- focus ^? Focus.decl <??> "test.Common.getFunctionGraph : Target is not a function"
    aliasInfo <- Pass.run1_ Struct.pass ast
    GraphBuilder.run aliasInfo decl


getExpr :: Enumerated a => Breadcrumbs -> Graph a v -> TModule V -> IO (TModule V)
getExpr bc graph ast = runPass $ do
    zipper <- lift $ eitherStringToM $ BCZipper.focusBreadcrumbs' bc ast
    let focus = BCZipper.getFocus zipper
    decl <- focus ^? Focus.decl <??> "test.Common.getExpr : Target is not a function"
    decl2 <- Pass.run2_ GraphParser.pass graph decl
    let newFocus = focus & Focus.decl .~ decl2
    return (BCZipper.close $ BCZipper.modify (const newFocus) zipper)


getMain :: (Show a, Show e) => LModule a e -> IO (LDecl a e)
getMain ast = eitherStringToM' $ runEitherT $ do
    focus <- hoistEither $ BCZipper.getFocus <$> BCZipper.focusBreadcrumbs' mainBC ast
    Focus.getFunction focus <??> "test.Common.getMain : Target is not a function"


runPass pass = do
    result <- Session.runT $ runEitherT pass
    eitherToM $ fst result

