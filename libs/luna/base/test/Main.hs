---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import           Control.Applicative
import           Control.Monad.RWS          hiding (join)
import           Control.Monad.State        hiding (join)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer       hiding (join)
import qualified Data.DList                 as DList
import           Data.Either.Utils          (forceEither)
import           Data.String.Utils          (join)
import           Data.Version               (Version (Version))
import           Debug.Trace
import           System.TimeIt

import qualified Flowbox.Luna.Data.AST.Crumb.Crumb                                       as ASTCrumb
import qualified Flowbox.Luna.Data.AST.Expr                                              as LExpr
import qualified Flowbox.Luna.Data.AST.Module                                            as FModule
import qualified Flowbox.Luna.Data.AST.Zipper.Focus                                      as Focus
import qualified Flowbox.Luna.Data.AST.Zipper.Zipper                                     as Zipper
import qualified Flowbox.Luna.Data.Cabal.Config                                          as Config
import qualified Flowbox.Luna.Data.Cabal.Section                                         as Section
import qualified Flowbox.Luna.Data.GraphView.GraphView                                   as GraphView
import qualified Flowbox.Luna.Data.HAST.Expr                                             as HExpr
import qualified Flowbox.Luna.Data.HAST.Module                                           as Module
import qualified Flowbox.Luna.Data.Pass.AliasInfo                                        as AliasInfo
import           Flowbox.Luna.Data.Pass.Source                                           (Source)
import qualified Flowbox.Luna.Data.Pass.Source                                           as Source
import qualified Flowbox.Luna.Data.PropertyMap                                           as PropertyMap
import qualified Flowbox.Luna.Passes.Analysis.Alias.Alias                                as Analysis.Alias
import qualified Flowbox.Luna.Passes.Analysis.CallGraph.CallGraph                        as Analysis.CallGraph
import qualified Flowbox.Luna.Passes.Analysis.FuncPool.FuncPool                          as FuncPool
import qualified Flowbox.Luna.Passes.Analysis.ID.MaxID                                   as MaxID
import qualified Flowbox.Luna.Passes.CodeGen.HSC.HSC                                     as HSC
import qualified Flowbox.Luna.Passes.General.Luna.Luna                                   as Luna
import qualified Flowbox.Luna.Passes.Source.File.Reader                                  as FileReader
import qualified Flowbox.Luna.Passes.Transform.AST.DepSort.DepSort                       as Transform.DepSort
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt
import qualified Flowbox.Luna.Passes.Transform.AST.Hash.Hash                             as Hash
import qualified Flowbox.Luna.Passes.Transform.AST.SSA.SSA                               as SSA
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser                      as Parser
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser                      as Parser
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser                   as TxtParser
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.Builder                     as GraphBuilder
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.Parser                       as GraphParser
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen                      as HASTGen
import           Flowbox.Prelude
import qualified Flowbox.System.Log.LogEntry                                             as LogEntry
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Log.Logger                                               as Logger
import qualified Flowbox.System.UniPath                                                  as UniPath
import           Flowbox.Text.Show.Hs                                                    (hsShow)
import qualified Flowbox.Text.Show.Pretty                                                as PP



genProject :: String -> Config.Config
genProject name = let
    exec = Section.mkExecutable name
    conf = Config.addSection exec
         $ Config.make name (Version [1] [])

    in conf

--main_inner :: IO (Either String ())
--main_inner = Luna.run $ do
--    let conf = genProject
--    putStrLn $ Config.genCode conf

--    return ()




logger :: LoggerIO
logger = getLoggerIO "Flowbox"


--example :: Source
--example = Source.Source ["Std", "Console"]
--        $ unlines [ ""
--                  , "class Console:"
--                  , "    def print self msg:"
--                  , "        ```print #{msg}```"
--                  ]

example :: Source
example = Source.Source ["Main"] $
        concat $ replicate 1 $ unlines [ ""

                        , "def print msg:"
                        , "    ```print' #{msg}```"




                    , "def main:"
                    , "    print $ test2 1"



                    , "def test2 a:"
                    , "    test1 a"


                    , "def test1 a:"
                    , "    {a,a}"

                    ]




main :: IO ()
main = do
    --DistMain.main
    Logger.setLevel DEBUG "Flowbox"
    --let x = Parser.parse' example
    --    --x :: Int

    --print $ x

    out <- timeIt main_inner
    case out of
        Right _ -> return ()
        Left  e -> putStrLn e

    --out <- timeIt main_graph
    --case out of
    --    Right _ -> return ()
    --    Left  e -> putStrLn e



main_inner :: IO (Either String ())
main_inner = Luna.run $ do
    let source = example

    logger info "\n-------- TxtParser --------"
    (ast, srcMap, astInfo) <- hoistEither =<< TxtParser.run source
    logger info "\n>> AST"
    logger info $ PP.ppqShow ast
    logger info "\n>> Source Map"
    logger info $ PP.ppShow srcMap
    logger info "\n>> AST Info"
    logger info $ PP.ppShow astInfo
    --logger info (show.length $ FModule._classes ast)
    return ()

    --let crumbs = [ASTCrumb.ModuleCrumb "Main", ASTCrumb.FunctionCrumb "add"]

    --let ast2 =     Zipper.mk ast
    --           >>= Zipper.focusFunction "add"
    --           >>= Zipper.modify (\(Zipper.FunctionFocus func) -> Zipper.FunctionFocus (func & LExpr.name .~ "dupa"))
    --           >>= Zipper.close

    --logger info $ PP.ppqShow ast2

    --putStrLn $ PP.ppShow zipper


    -- Should be run BEFORE Analysis.Alias
    logger info "\n-------- Desugar.ImplicitSelf --------"
    (ast, astInfo) <- hoistEither =<< Desugar.ImplicitSelf.run astInfo ast
    logger info $ PP.ppqShow ast


    logger info "\n-------- Desugar.TLRecUpdt --------"
    (ast, astInfo) <- hoistEither =<< Desugar.TLRecUpdt.run astInfo ast
    logger info $ PP.ppqShow ast


    logger info "\n-------- Analysis.Alias --------"
    aliasInfo <- hoistEither =<< Analysis.Alias.run ast
    logger info "\n>> varRel:"
    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.varRel)
    logger info "\n>> aliasMap:"
    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.aliasMap)
    logger info "\n>> invalidMap:"
    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.invalidMap)
    --logger info "\n>> parentMap"
    --logger info $ PP.ppShow (aliasInfo ^. AliasInfo.invalidMap)

    -----------------------------------------
    -- !!! CallGraph and DepSort are mockup passes !!!
    -- They are working right now only with not self typed variables
    logger info "\n-------- Analysis.CallGraph --------"
    callGraph <- hoistEither =<< Analysis.CallGraph.run aliasInfo ast
    logger info $ PP.ppShow callGraph


    logger info "\n-------- Transform.DepSort --------"
    ast <- hoistEither =<< Transform.DepSort.run callGraph aliasInfo ast
    logger info $ PP.ppShow ast
    -----------------------------------------


    -- !!! [WARNING] INVALIDATES aliasInfo !!!
    logger info "\n-------- Desugar.ImplicitScopes --------"
    (ast, astInfo) <- hoistEither =<< Desugar.ImplicitScopes.run astInfo aliasInfo ast
    logger info $ PP.ppqShow ast


    -- Should be run AFTER ImplicitScopes
    logger info "\n-------- Desugar.ImplicitCalls --------"
    (ast, astInfo) <- hoistEither =<< Desugar.ImplicitCalls.run astInfo ast
    logger info $ PP.ppqShow ast


    logger info "\n-------- Analysis.Alias --------"
    aliasInfo <- hoistEither =<< Analysis.Alias.run ast
    logger info "\n>> varRel:"
    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.varRel)
    logger info "\n>> aliasMap:"
    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.aliasMap)
    logger info "\n>> invalidMap:"
    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.invalidMap)





    --logger info "\n-------- FuncPool --------"
    --fp <- hoistEither =<< FuncPool.run ast
    --logger info $ PP.ppShow fp

    logger info "\n-------- Hash --------"
    hash <- hoistEither =<< Hash.run ast
    --logger info $ PP.ppShow hash

    logger info "\n-------- SSA --------"
    ssa <- hoistEither =<< SSA.run aliasInfo hash
    --logger info $ PP.ppqShow ssa

    logger info "\n-------- HASTGen --------"
    hast <- hoistEither =<< HASTGen.run ssa
    --logger info $ PP.ppShow hast

    logger info "\n-------- HSC --------"
    hsc <- hoistEither =<< HSC.run  hast
    logger info $ join "\n\n" (map printSrc hsc)


    return ()


main_graph :: IO (Either String ())
main_graph = Luna.run $ do
    --let source  = example
    --    emptyPM = PropertyMap.empty

    --logger info "\n-------- TxtParser --------"
    --(ast, _) <- hoistEither =<< TxtParser.run source
    --logger info $ PP.ppqShow ast

    --logger info "\n-------- VarAlias --------"
    --va <- hoistEither =<< VarAlias.runGather ast
    --logger info $ PP.ppShow va

    --(Focus.FunctionFocus expr) <- Zipper.mk ast
    --                          >>= Zipper.focusFunction "test"
    --                          >>= return . Zipper.getFocus

    --logger info $ PP.ppShow expr
    --(graph, pm) <- hoistEither =<< GraphBuilder.run va emptyPM expr
    --let graphView = GraphView.fromGraph graph
    --    (graphWithDefaults, pmWithDefaults) = Defaults.addDefaults graphView pm
    --logger warning $ show graph
    --logger warning $ PP.ppShow pm
    ----logger info $ show graphWithDefaults
    --let (newGraphView, newPM) = Defaults.removeDefaults graphWithDefaults pmWithDefaults
    --newGraph <- GraphView.toGraph newGraphView
    ----logger warning $ show newGraph
    --expr' <- hoistEither =<< GraphParser.run newGraph newPM expr
    --logger info $ PP.ppShow expr'
    --logger warning $ PP.ppShow newPM

    --logger info "\n-------- FuncPool --------"
    --fp <- FuncPool.run ast
    --logger info $ PP.ppShow fp

    --logger info "\n-------- SSA --------"
    --ssa <- SSA.run va ast
    ----logger info $ PP.ppqShow ssa

    --logger info "\n-------- HASTGen --------"
    --hast <- HASTGen.run ssa fp
    --logger info $ PP.ppShow hast

    --logger info "\n-------- HSC --------"
    --hsc <- HSC.run  hast
    --logger info $ join "\n\n" (map printSrc hsc)


    return ()


printSrc :: Source -> [Char]
printSrc src = ">>> file '" ++ join "/" (Source.path src) ++ "':\n\n"
             ++ hsShow (Source.code src)


