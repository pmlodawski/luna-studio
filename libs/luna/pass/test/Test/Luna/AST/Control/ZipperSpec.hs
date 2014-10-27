---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.AST.Control.ZipperSpec where

import Test.Hspec

import           Flowbox.Control.Error
import           Flowbox.Prelude
import qualified Luna.AST.Control.Crumb  as Crumb
import qualified Luna.AST.Control.Focus  as Focus
import qualified Luna.AST.Control.Zipper as Zipper
import qualified Luna.AST.Expr           as Expr
import           Luna.AST.Module         (Module)
import qualified Luna.AST.Name           as Name
import qualified Test.Luna.AST.Common    as Common
import qualified Test.Luna.Sample.Code   as SampleCode



main :: IO ()
main = hspec spec


getAST :: IO Module
getAST = Common.getAST SampleCode.zipperTestModule


spec :: Spec
spec = do
    describe "AST zippers" $ do
        it "focus and defocus on function in module" $ do
            ast <- getAST
            zipper <- eitherToM $ Zipper.focusBreadcrumbs'
                        [ Crumb.Module   "Main"
                        , Crumb.Function (Name.single "main") []
                        ] ast
            let focus = Zipper.getFocus  zipper
            Expr.Function {} <- Focus.getFunction focus <?.> "Not a function"
            Zipper.close (Zipper.defocus zipper) `shouldBe` ast

        it "focus and defocus on class in module" $ do
            ast    <- getAST
            zipper <- eitherToM $ Zipper.focusBreadcrumbs'
                        [ Crumb.Module   "Main"
                        , Crumb.Class    "Vector"
                        ] ast
            let focus = Zipper.getFocus  zipper
            Expr.Data {} <- Focus.getClass focus <?.> "Not a class"
            Zipper.close (Zipper.defocus zipper) `shouldBe` ast

        it "focus and defocus on class in class in module" $ do
            ast    <- getAST
            zipper <- eitherToM $ Zipper.focusBreadcrumbs'
                        [ Crumb.Module   "Main"
                        , Crumb.Class    "Vector"
                        , Crumb.Class    "Inner"
                        ] ast
            let focus = Zipper.getFocus  zipper
            Expr.Data {} <- Focus.getClass focus <?.> "Not a class"
            Zipper.close (Zipper.defocus zipper) `shouldBe` ast

        it "focus and defocus on function in class in module" $ do
            ast    <- getAST
            zipper <- eitherToM $ Zipper.focusBreadcrumbs'
                        [ Crumb.Module   "Main"
                        , Crumb.Class    "Vector"
                        , Crumb.Function (Name.single "test") []
                        ] ast
            let focus = Zipper.getFocus  zipper
            Expr.Function {} <- Focus.getFunction focus <?.> "Not a function"
            Zipper.close (Zipper.defocus zipper) `shouldBe` ast

        it "focus and defocus on function in class in class in module" $ do
            ast    <- getAST
            zipper <- eitherToM $ Zipper.focusBreadcrumbs'
                        [ Crumb.Module   "Main"
                        , Crumb.Class    "Vector"
                        , Crumb.Class    "Inner"
                        , Crumb.Function (Name.single "inner") []
                        ] ast
            let focus = Zipper.getFocus  zipper
            Expr.Function {} <- Focus.getFunction focus <?.> "Not a function"
            Zipper.close (Zipper.defocus zipper) `shouldBe` ast

        it "focus and defocus on lambda in function in class in module" $ do
            ast    <- getAST
            zipper <- eitherToM $ Zipper.focusBreadcrumbs'
                        [ Crumb.Module   "Main"
                        , Crumb.Class    "Vector"
                        , Crumb.Function (Name.single "test") []
                        , Crumb.Lambda   19
                        ] ast
            let focus = Zipper.getFocus  zipper
            Expr.Lambda {} <- Focus.getLambda focus <?.> "Not a lambda"
            Zipper.close (Zipper.defocus zipper) `shouldBe` ast
