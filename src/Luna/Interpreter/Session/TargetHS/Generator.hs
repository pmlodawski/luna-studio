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
import qualified Data.List                   as List
import qualified Data.Text.Lazy              as Text
import qualified Language.Preprocessor.Cpphs as Cpphs

import           Flowbox.Control.Error                  (hoistEitherWith, lmapEitherT)
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
import qualified Luna.Interpreter.Session.Error         as Error
import           Luna.Interpreter.Session.Session       (Session)
import qualified Luna.Parser.Parser                     as Parser
import qualified Luna.Parser.Pragma                     as Pragma
import qualified Luna.Pass                              as Pass
import qualified Luna.Pass.Target.HS.HASTGen            as HASTGen
import qualified Luna.Pass.Target.HS.HSC                as HSC
import qualified Luna.Pass.Transform.SSA                as SSA
import           Luna.Syntax.Enum                       (IDTag)
import           Luna.Syntax.Expr                       (LExpr)
import           Luna.Syntax.Module                     (LModule)
import           Luna.Syntax.Unit                       (Unit (Unit))
import qualified Luna.System.Pragma.Store               as Pragma
import           Luna.System.Session                    as Session



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


emptyModule :: Module
emptyModule = Module.mk def $ Type.Module def "Main" []


genAll :: Session mm String
genAll = do
    mainPtr <- Env.getMainPtr
    ast     <- Env.getModule $ (DefPoint.breadcrumbs %~ init) mainPtr
    genCode (tail . tail . dropWhile (not . (== "-- body --"))) ast


genClass :: DefPoint -> Session mm String
genClass defPoint = do
    expr <- Env.getClass defPoint
    let ast = emptyModule & Module.classes .~ [expr]
    genCode (tail . tail . dropWhile (not . (== "-- body --"))) ast


genFunctions :: Session mm String
genFunctions = do
    --expr <- Session.getFunction defPoint
    --let ast = emptyModule & Module.methods .~ [expr]
    mainPtr <- Env.getMainPtr
    ast     <- Env.getModule $ (DefPoint.breadcrumbs %~ init) mainPtr
    genCode ( List.filter (not . (\a -> List.isPrefixOf "data " a || List.isPrefixOf "$(generateFieldAccessors" a))
             . tail . tail . dropWhile   (not . (== "-- body --"))
            --dropWhile   (not . (== "-- ====== Method: Vector.test ====== --"))

            ) ast

runP :: (Functor m, Show e) => EitherT e m b -> EitherT Error.Error m b
runP = lmapEitherT (Error.OtherError $(loc) . show)

type NewAST = Unit (LModule IDTag (LExpr IDTag ()))


genCode :: ([String] -> [String]) -> Module -> Session mm String
genCode selector oldAst = do
    ast <- Unit <$> runP (convertAST oldAst)
    result <- Session.runT $ do
        void   Parser.init
        void $ Pragma.enable (Pragma.orphanNames)
        void $ Pragma.pop    (Pragma.orphanNames)
        runEitherT $ do
            ast  <- Pass.run1_ SSA.pass     (ast :: NewAST)
            hast <- Pass.run1_ HASTGen.pass (ast :: NewAST)
            Pass.run1_ HSC.pass hast
    cpphsOptions <- Env.getCpphsOptions
    hsc <- hoistEitherWith (Error.OtherError $(loc) . show) $ fst result
    liftIO $ Cpphs.runCpphs cpphsOptions "" $ unlines $ selector $ lines $ Text.unpack hsc


