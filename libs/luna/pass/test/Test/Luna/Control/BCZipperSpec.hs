---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Test.Luna.Control.BCZipperSpec where

import           Test.Hspec

import           Flowbox.Control.Error
import           Flowbox.Prelude
import qualified Luna.Control.BCZipper as BCZipper
import qualified Luna.Control.Crumb    as Crumb
import qualified Luna.Control.Focus    as Focus
import qualified Luna.Syntax.Decl      as Decl
import qualified Luna.Syntax.Enum      as Enum
import           Luna.Syntax.Expr      (LExpr)
import           Luna.Syntax.Label     (Label (Label))
import           Luna.Syntax.Module    (LModule)
import qualified Test.Luna.Sample.Code as SampleCode
import qualified Test.Luna.Syntax.AST  as AST


main :: IO ()
main = hspec spec


getAST :: IO (LModule Enum.IDTag (LExpr Enum.IDTag ()))
getAST = fst <$> AST.getAST SampleCode.zipperTestModule


--zipTo :: (a -> Bool) -> BCZipper h j [a] -> Maybe (BCZipper h j [a] :>> a)
--zipTo predicate z = do
--    i <- List.findIndex predicate $ z ^. focus
--    fromWithin traverse z & moveTo i

spec :: Spec
spec = do
    describe "AST zippers" $ do
        --it "" pending
        it "focus and defocus on function in module" $ do
            ast <- getAST
            --let z = zipper ast
            --      & downward (Label.element . Module.body)
            --    z' = fromWithin traverse z
            --        & moveTo 1
            --       <&> view focus

            --    predicate = const True

            --prettyPrint $ view focus $ z
            --putStrLn "==============="
            --prettyPrint $ f z'
            --putStrLn "==============="
            --prettyPrint $ f (zipTo predicate z <&> view focus)

            --pending
            zipper <- eitherToM $ BCZipper.focusBreadcrumbs'
                        [ Crumb.Module   "Main"
                        , Crumb.Function [] "main"
                        ] ast
            let focus = BCZipper.getFocus  zipper
            (Label _ Decl.Func {}) <- Focus.getFunction focus <?.> "Not a function"
            BCZipper.close (BCZipper.defocus zipper) `shouldBe` ast

        it "focus and defocus on class in module" $ do
            ast    <- getAST
            zipper <- eitherToM $ BCZipper.focusBreadcrumbs'
                        [ Crumb.Module   "Main"
                        , Crumb.Class    "Vector"
                        ] ast
            let focus = BCZipper.getFocus  zipper
            (Label _ Decl.Data {}) <- Focus.getData focus <?.> "Not a class"
            BCZipper.close (BCZipper.defocus zipper) `shouldBe` ast

        it "focus and defocus on class in class in module" $ do
            ast    <- getAST
            zipper <- eitherToM $ BCZipper.focusBreadcrumbs'
                        [ Crumb.Module   "Main"
                        , Crumb.Class    "Vector"
                        , Crumb.Class    "Inner"
                        ] ast
            let focus = BCZipper.getFocus  zipper
            (Label _ Decl.Data {}) <- Focus.getData focus <?.> "Not a class"
            BCZipper.close (BCZipper.defocus zipper) `shouldBe` ast

        it "focus and defocus on function in class in module" $ do
            ast    <- getAST
            zipper <- eitherToM $ BCZipper.focusBreadcrumbs'
                        [ Crumb.Module   "Main"
                        , Crumb.Class    "Vector"
                        , Crumb.Function [] "test"
                        ] ast
            let focus = BCZipper.getFocus  zipper
            (Label _ Decl.Func {}) <- Focus.getFunction focus <?.> "Not a function"
            BCZipper.close (BCZipper.defocus zipper) `shouldBe` ast

        it "focus and defocus on function in class in class in module" $ do
            ast    <- getAST
            zipper <- eitherToM $ BCZipper.focusBreadcrumbs'
                        [ Crumb.Module   "Main"
                        , Crumb.Class    "Vector"
                        , Crumb.Class    "Inner"
                        , Crumb.Function [] "inner"
                        ] ast
            let focus = BCZipper.getFocus  zipper
            (Label _ Decl.Func {}) <- Focus.getFunction focus <?.> "Not a function"
            BCZipper.close (BCZipper.defocus zipper) `shouldBe` ast

        it "focus and defocus on lambda in function in class in module" $ do
            pendingWith "not implemented"
            --ast    <- getAST
            --zipper <- eitherToM $ BCZipper.focusBreadcrumbs'
            --            [ Crumb.Module   "Main"
            --            , Crumb.Class    "Vector"
            --            , Crumb.Function [] "test"
            --            , Crumb.Lambda   19
            --            ] ast
            --let focus = BCZipper.getFocus  zipper
            --(Label _ Decl.Lambda {}) <- Focus.getLambda focus <?.> "Not a lambda"
            --BCZipper.close (BCZipper.defocus zipper) `shouldBe` ast
