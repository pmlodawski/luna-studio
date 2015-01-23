---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators #-}

module Test.Luna.Syntax.Control.ZipperSpec where

import           Control.Zipper
import qualified Data.List      as List
import           Test.Hspec

import           Flowbox.Prelude
import qualified Luna.Syntax.Enum        as Enum
import           Luna.Syntax.Expr        (LExpr)
import qualified Luna.Syntax.Label       as Label
import           Luna.Syntax.Module      (LModule)
import qualified Luna.Syntax.Module      as Module
import qualified Test.Luna.Sample.Code   as SampleCode
import qualified Test.Luna.Syntax.Common as Common
--import           Flowbox.Control.Error
--import qualified Luna.Syntax.Control.Crumb  as Crumb
--import qualified Luna.Syntax.Control.Focus  as Focus
--import qualified Luna.Syntax.Control.Zipper as Zipper
--import qualified Luna.AST.Name           as Name
--import qualified Luna.Syntax.Expr        as Expr


f :: Maybe a -> Maybe a
f = id

main :: IO ()
main = hspec spec


getAST :: IO (LModule Enum.IDTag (LExpr Enum.IDTag ()))
getAST = fst <$> Common.getAST SampleCode.zipperTestModule


zipTo :: (a -> Bool) -> Zipper h j [a] -> Maybe (Zipper h j [a] :>> a)
zipTo predicate z = do
    i <- List.findIndex predicate $ z ^. focus
    fromWithin traverse z & moveTo i

spec :: Spec
spec = do
    describe "AST zippers" $ do
        --it "" pending
        it "focus and defocus on function in module" $ do
            ast <- getAST
            let z = zipper ast
                  & downward (Label.element . Module.body)
                z' = fromWithin traverse z
                    & moveTo 1
                   <&> view focus

                predicate = const True

            prettyPrint $ view focus $ z
            putStrLn "==============="
            prettyPrint $ f z'
            putStrLn "==============="
            prettyPrint $ f (zipTo predicate z <&> view focus)

            pending
    --        zipper <- eitherToM $ Zipper.focusBreadcrumbs'
    --                    [ Crumb.Module   "Main"
    --                    , Crumb.Function (Name.single "main") []
    --                    ] ast
    --        let focus = Zipper.getFocus  zipper
    --        Expr.Function {} <- Focus.getFunction focus <?.> "Not a function"
    --        Zipper.close (Zipper.defocus zipper) `shouldBe` ast

    --    it "focus and defocus on class in module" $ do
    --        ast    <- getAST
    --        zipper <- eitherToM $ Zipper.focusBreadcrumbs'
    --                    [ Crumb.Module   "Main"
    --                    , Crumb.Class    "Vector"
    --                    ] ast
    --        let focus = Zipper.getFocus  zipper
    --        Expr.Data {} <- Focus.getClass focus <?.> "Not a class"
    --        Zipper.close (Zipper.defocus zipper) `shouldBe` ast

    --    it "focus and defocus on class in class in module" $ do
    --        ast    <- getAST
    --        zipper <- eitherToM $ Zipper.focusBreadcrumbs'
    --                    [ Crumb.Module   "Main"
    --                    , Crumb.Class    "Vector"
    --                    , Crumb.Class    "Inner"
    --                    ] ast
    --        let focus = Zipper.getFocus  zipper
    --        Expr.Data {} <- Focus.getClass focus <?.> "Not a class"
    --        Zipper.close (Zipper.defocus zipper) `shouldBe` ast

    --    it "focus and defocus on function in class in module" $ do
    --        ast    <- getAST
    --        zipper <- eitherToM $ Zipper.focusBreadcrumbs'
    --                    [ Crumb.Module   "Main"
    --                    , Crumb.Class    "Vector"
    --                    , Crumb.Function (Name.single "test") []
    --                    ] ast
    --        let focus = Zipper.getFocus  zipper
    --        Expr.Function {} <- Focus.getFunction focus <?.> "Not a function"
    --        Zipper.close (Zipper.defocus zipper) `shouldBe` ast

    --    it "focus and defocus on function in class in class in module" $ do
    --        ast    <- getAST
    --        zipper <- eitherToM $ Zipper.focusBreadcrumbs'
    --                    [ Crumb.Module   "Main"
    --                    , Crumb.Class    "Vector"
    --                    , Crumb.Class    "Inner"
    --                    , Crumb.Function (Name.single "inner") []
    --                    ] ast
    --        let focus = Zipper.getFocus  zipper
    --        Expr.Function {} <- Focus.getFunction focus <?.> "Not a function"
    --        Zipper.close (Zipper.defocus zipper) `shouldBe` ast

    --    it "focus and defocus on lambda in function in class in module" $ do
    --        ast    <- getAST
    --        zipper <- eitherToM $ Zipper.focusBreadcrumbs'
    --                    [ Crumb.Module   "Main"
    --                    , Crumb.Class    "Vector"
    --                    , Crumb.Function (Name.single "test") []
    --                    , Crumb.Lambda   19
    --                    ] ast
    --        let focus = Zipper.getFocus  zipper
    --        Expr.Lambda {} <- Focus.getLambda focus <?.> "Not a lambda"
    --        Zipper.close (Zipper.defocus zipper) `shouldBe` ast
