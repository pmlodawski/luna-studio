---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Luna.Interpreter.Session.TargetHS.Generator where

import           Control.Monad.Trans.Either
import qualified Data.List                              as List
import qualified Data.Text.Lazy                         as Text
import qualified Language.Preprocessor.Cpphs            as Cpphs

import           Flowbox.Control.Error                  (hoistEitherWith)
import           Flowbox.Prelude
import           Flowbox.Source.Location                (loc)
import           Flowbox.System.Log.Logger
import           Luna.DEP.AST.ASTConvertion             (convertAST)
import           Luna.DEP.AST.Module                    (Module)
import qualified Luna.DEP.AST.Module                    as Module
import qualified Luna.DEP.AST.Type                      as Type
import           Luna.Interpreter.Session.Data.DefPoint (DefPoint)
import qualified Luna.Interpreter.Session.Data.DefPoint as DefPoint
import qualified Luna.Interpreter.Session.Env           as Env
import           Luna.Interpreter.Session.Error         (mapError)
import qualified Luna.Interpreter.Session.Error         as Error
import           Luna.Interpreter.Session.Session       (Session)
import qualified Luna.Parser.Parser                     as Parser
import qualified Luna.Pass                              as Pass
import qualified Luna.Pass.Target.HS.HASTGen            as HASTGen
import qualified Luna.Pass.Target.HS.HSC                as HSC
import           Luna.Syntax.Enum                       (IDTag)
import           Luna.Syntax.Expr                       (LExpr)
import           Luna.Syntax.Module                     (LModule)
import           Luna.Syntax.Unit                       (Unit (Unit))
import           Luna.System.Session                    as Session



logger :: LoggerIO
logger = getLoggerIO $moduleName


emptyModule :: Module
emptyModule = Module.mk def $ Type.Module def "Main" []


genAll :: Session mm String
genAll = do
    mainPtr <- Env.getMainPtr
    ast'     <- Env.getModule $ (DefPoint.breadcrumbs %~ init) mainPtr
    let ast = emptyModule & Module.classes .~ (ast' ^. Module.classes)
    genCode (takeWhile (not . (== "-- Main module wrappers")) . tail . tail . dropWhile (not . (== "-- body --"))) ast
    --mainPtr <- Env.getMainPtr
    --ast     <- Env.getModule $ (DefPoint.breadcrumbs %~ init) mainPtr
    --genCode (tail . tail . dropWhile (not . (== "-- body --"))) ast


genClass :: DefPoint -> Session mm String
genClass defPoint = do
    expr <- Env.getClass defPoint
    let ast = emptyModule & Module.classes .~ [expr]
    genCode (takeWhile (not . (== "-- Main module wrappers")) . tail . tail . dropWhile (not . (== "-- body --"))) ast
    --expr <- Env.getClass defPoint
    --let ast = emptyModule & Module.classes .~ [expr]
    --genCode (tail . tail . dropWhile (not . (== "-- body --"))) ast


genFunctions :: Session mm String
genFunctions = do
    mainPtr <- Env.getMainPtr
    ast     <- Env.getModule $ (DefPoint.breadcrumbs %~ init) mainPtr
    genCode ( List.filter (not . (\a -> List.isPrefixOf "data " a || List.isPrefixOf "$(generateFieldAccessors" a))
            . tail . tail . dropWhile   (not . (== "-- body --"))
            ) ast


genCode :: ([String] -> [String]) -> Module -> Session mm String
genCode selector oldAst = do
    ast <- Unit <$> mapError $(loc) (convertAST oldAst)
    result <- Session.runT $ do
        void Parser.init
        runEitherT $ do
            hast <- Pass.run2_ HASTGen.pass mempty (ast :: Unit (LModule IDTag (LExpr IDTag ())))
            Pass.run1_ HSC.pass hast
    cpphsOptions <- Env.getCpphsOptions
    hsc <- hoistEitherWith (Error.OtherError $(loc) . show) $ fst result
    liftIO $ Cpphs.runCpphs cpphsOptions "" $ unlines $ selector $ lines $ Text.unpack hsc


