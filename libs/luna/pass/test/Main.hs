---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

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

import           Flowbox.Prelude
import qualified Flowbox.System.Log.LogEntry                                   as LogEntry
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Log.Logger                                     as Logger
import qualified Flowbox.System.UniPath                                        as UniPath
import           Flowbox.Text.Show.Hs                                          (hsShow)
import qualified Flowbox.Text.Show.Pretty                                      as PP
import qualified Luna.AST.Control.Crumb                                        as ASTCrumb
import qualified Luna.AST.Control.Focus                                        as Focus
import qualified Luna.AST.Control.Zipper                                       as Zipper
import qualified Luna.AST.Expr                                                 as LExpr
import qualified Luna.AST.Module                                               as FModule
import qualified Luna.Data.AliasInfo                                           as AliasInfo
import qualified Luna.Data.HAST.Expr                                           as HExpr
import qualified Luna.Data.HAST.Module                                         as Module
import           Luna.Data.Source                                              (Source)
import qualified Luna.Data.Source                                              as Source
import qualified Luna.Distribution.Cabal.Config                                as Config
import qualified Luna.Distribution.Cabal.Section                               as Section
import qualified Luna.Graph.PropertyMap                                        as PropertyMap
import qualified Luna.Parser.Parser                                            as Parser
import qualified Luna.Parser.Parser                                            as Parser
import qualified Luna.Pass.Analysis.Alias.Alias                                as Analysis.Alias
import qualified Luna.Pass.Analysis.CallGraph.CallGraph                        as Analysis.CallGraph
import qualified Luna.Pass.Analysis.FuncPool.FuncPool                          as FuncPool
import qualified Luna.Pass.Analysis.ID.MaxID                                   as MaxID
import qualified Luna.Pass.CodeGen.HSC.HSC                                     as HSC
import qualified Luna.Pass.General.Luna.Luna                                   as Luna
import qualified Luna.Pass.Source.File.Reader                                  as FileReader
import qualified Luna.Pass.Transform.AST.DepSort.DepSort                       as Transform.DepSort
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
import qualified Luna.Pass.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt
import qualified Luna.Pass.Transform.AST.Hash.Hash                             as Hash
import qualified Luna.Pass.Transform.AST.SSA.SSA                               as SSA
import qualified Luna.Pass.Transform.AST.TxtParser.TxtParser                   as TxtParser
import qualified Luna.Pass.Transform.HAST.HASTGen.HASTGen                      as HASTGen



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
logger = getLoggerIO $(moduleName)


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
                    --, "@AllowOrphans"
                    , "def print msg:"
                    , "    ```polyJoin . liftF1 (Value . fmap Safe . print) $ #{msg}```"

                    ----, "class Vector:"
                    ----, "    x,y,z :: [Vector]"
                    --, "def Int.+ a:"
                    --, "   a"
                    , "class ```Maybe``` a:"
                    , "    Just: unwrap :: a"
                    , "    Nothing"
                    --, "def foo a b:"
                    --, "    print a"
                    --, "    print b"

                    , "def main:"
                    --, "    a = Vector [] [] []"
                    --, "    print [1..5]"
                    --, "    foo 1 2"
                    , "    print $ Just 5"
                    ]


        --concat $ replicate 1 $ unlines [ ""
        --            --, "@AllowOrphans"
        --            , "class Vector a:"
        --            , "    Vector: x,y,z :: a"
        --            , "    Scalar: w     :: a"

        --            , "def print msg:"
        --            , "    ```polyJoin . liftF1 (Value . fmap Safe . print) $ #{msg}```"


        --            , "def main:"
        --            , "    v = Vector 1 2 3"
        --            , "    s = Scalar 5"
        --            , "    Vector a b c = v"
        --            , "    print a"
        --            ]




main :: IO ()
main = do
    --DistMain.main
    Logger.setLevel DEBUG ""
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
    logger info "\n>> scope:"
    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.scope)
    logger info "\n>> alias map:"
    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.alias)
    logger info "\n>> parent map:"
    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.parent)
    logger info "\n>> orphans map:"
    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.orphans)
    --logger info "\n>> parentMap"
    --logger info $ PP.ppShow (aliasInfo ^. AliasInfo.orphans)

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
    logger info "\n>> scope:"
    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.scope)
    logger info "\n>> alias map:"
    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.alias)
    logger info "\n>> orphans map:"
    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.orphans)





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
    logger info $ PP.ppShow hast

    logger info "\n-------- HSC --------"
    hsc <- hoistEither =<< HSC.run  hast
    logger info $ join "\n\n" (map showSrc hsc)


    return ()


showSrc :: Source -> String
showSrc src = ">>> file '" ++ join "/" (src ^. Source.path) ++ "':\n\n"
             ++ hsShow (src ^. Source.code)
