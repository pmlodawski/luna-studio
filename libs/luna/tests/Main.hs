---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import           Control.Applicative
import           Control.Lens               hiding (Zipper)
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

import qualified Flowbox.Distribution.M                                    as DistMain
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb                         as ASTCrumb
import qualified Flowbox.Luna.Data.AST.Expr                                as LExpr
import qualified Flowbox.Luna.Data.AST.Module                              as FModule
import qualified Flowbox.Luna.Data.AST.Zipper.Focus                        as Focus
import qualified Flowbox.Luna.Data.AST.Zipper.Zipper                       as Zipper
import qualified Flowbox.Luna.Data.Cabal.Config                            as Config
import qualified Flowbox.Luna.Data.Cabal.Section                           as Section
import qualified Flowbox.Luna.Data.GraphView.GraphView                     as GraphView
import qualified Flowbox.Luna.Data.HAST.Expr                               as HExpr
import qualified Flowbox.Luna.Data.HAST.Module                             as Module
import qualified Flowbox.Luna.Data.PropertyMap                             as PropertyMap
import           Flowbox.Luna.Data.Source                                  (Source)
import qualified Flowbox.Luna.Data.Source                                  as Source
import qualified Flowbox.Luna.Passes.Analysis.FuncPool.FuncPool            as FuncPool
import qualified Flowbox.Luna.Passes.Analysis.ID.MaxID                     as MaxID
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias            as VarAlias
import qualified Flowbox.Luna.Passes.CodeGen.HSC.HSC                       as HSC
import qualified Flowbox.Luna.Passes.General.Luna.Luna                     as Luna
import qualified Flowbox.Luna.Passes.Source.File.Reader                    as FileReader
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser        as Parser
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser     as TxtParser
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.Builder       as GraphBuilder
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.Parser         as GraphParser
import qualified Flowbox.Luna.Passes.Transform.GraphView.Defaults.Defaults as Defaults
import qualified Flowbox.Luna.Passes.Transform.GraphView.Defaults.Defaults as Defaults
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen        as HASTGen
import qualified Flowbox.Luna.Passes.Transform.SSA.SSA                     as SSA
import qualified Flowbox.Luna.Passes.Transform.Hash.Hash                   as Hash
import           Flowbox.Prelude
import qualified Flowbox.System.Log.LogEntry                               as LogEntry
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Log.Logger                                 as Logger
import qualified Flowbox.System.UniPath                                    as UniPath
import           Flowbox.Text.Show.Hs                                      (hsShow)
import qualified Flowbox.Text.Show.Pretty                                  as PP

import qualified Flowbox.Luna.Data.AST.Crumb.Crumb as ASTCrumb

import Control.Lens hiding (Zipper)

import qualified Flowbox.Distribution.M as DistMain

import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser as Parser

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
                    --, "import Std:Vector"
                    --, "def List.length self:"
                    --, "    ```getIO $ liftFPure1 length #{self}```"
                    --, "def List.each self callback:"
                    --, "    ```let {mymap x (Pure y) = mapM x y}```"
                    --, "    ```getIO $ mymap (get1 #{callback}) #{self}```"
                        --, "def List.each self callback:"
                        --, "    ```let {mymap = liftf2 map}```"
                        --, "    ```mymap (val $ call1 #{callback}) #{self}```"

                        --, "def List.at self index:"
                        --, "    ```(flattenCtx `dot2` liftf2 (!!)) #{self} #{index}```"
                        ----, "def Int.add a b:"
                        ----, "    ```getIO $ liftFPure2 (+) #{a} #{b}```"
                        ----, "def Int.sub a b:"
                        ----, "    ```getIO $ liftFPure2 (-) #{a} #{b}```"
                        ----, "def Int.mul a b:"
                        ----, "    ```getIO $ liftFPure2 (*) #{a} #{b}```"
                        ----, "def List.add self x:"
                        ----, "    ```getIO $ liftFPure2 (++) #{self} #{x}```"
                        , "class Console:"
                        , "    def print self msg:"
                        , "        ```print' #{msg}```"

                        --, "def Int.+ a b:"
                        --, "    ```liftf2 (+) #{a} #{b}```"
                        ----, "class Vector a b c:"
                        ----, "    x :: a"
                        ----, "class Vector a = Vector | Scalar "
                        ----, "               | Scalar2"
                        ----, "class Vector a = Vector: x :: a"
                        ----, "                         y :: a"
                        ----, "                         z :: a"
                        ----, "               | Scalar: a :: a"
                        ----, "               | Vector2: x :: a"
                        ----, "               | Scalar"
                        , "class Vector a:"
                        , "    Vector: x,y,z :: a"
                        --, "    def test self:"
                        --, "        self"
                        --, "    def + self v2:"
                        --, "        Vector (self.x()+v2.x()) (self.y()+v2.y()) (self.z()+v2.z())"
                        ----, "    Scalar: a     :: a"
                        ----, "    def f self (x::Int):"
                        ----, "       1"
                        ----, "def f"
                        ----, "def g"
                        ----, "    def test self x: {self,5}"
                        ----, "    def test2 self x: {self,5}"
      
                        ----, "def main self:"
                        ----, "    v = Vector 1 2 3"
                        ----, "    v"
                        ----, "    (2+2).f"
      
                        ----, "def fxx (y::Int):"
                        ----, "    a = b + (c)"
                        ----, "    Vector"
                        ----, "    def g:"
                        ----, "        xxx"
                        ----, "    b"
                        ----, "    b x:"
                        ----, "        x+1"
                        ----, "def fyy x:"
                        ----, "    b"
                        ----, "    def h:"
                        ----, "        yyy"
                        ----, "    a = case x:"
                        ----, "        a: a"
                        ----, "        {a,b} : 1"
                        ----, "    v.x.y"
                        --, "def main self:"
                        --, "    c = Console()"
                        ----, "    d = [1..10].each x:"
                        ----, "       Console().print (x*2)"
                        ----, "    c.print d"
                        --, "    c.print $ [1..100].at 5"
                    --, "    c = Console()"
                    --, "    p = c.print"
                    --, "    f (5)"
                    --, "    p = Console().print"
                    --, "    o = [1,2,3].each x:"
                    --, "       p.print x"
                    --, "    p 1"
                    --, "    p \"ala\""
                    --, "    a.throw"
  
  
                    --, "    Console().print $ v.test 1"
                    --, "def Vector.vtest self a b:"
                    --, "    {a,b}"
                    --, "def test self a b:"
                    --, "    a + b"
                    --, "    a = x: x.add 5"
                    --, "    v = Vector 1 2 3"
                    --, "    Console.print ([1,2,30..0].length)"
  
                    --, "    Console.print (1.add 2)"
  
                    , "def main self:"
                    , "   c = Console()"
                    --, "   c.print (2+2)"
                    , "   v = Vector 1 2 3"
                    --, "   v.x = 10"
                    , "   c.print v"
                    --, "   c.print (v+v)"
                    ----, "    Console.print {1}"
                    --, "    x = [1,2,3]"
                    --, "    Console.print x"
                    --, "    y = x.each el:"
                    --, "        Console.print el"
                    ----, "        el.add 1"
                    --, "    Console.print \"hello\""
  
  
  
                    --, "def add self x y:"
                    --, "   x.add y"
  
                    --, "def add2 self x y:"
                    --, "   x.add y"
                    --, "def addInts(self, x, y) -> Int:"
                    --, "   x.add y"
                    --, "def main self:"
                    --, "    Console.print 1"
                    --, "   [1..10].each x:"
                    --, "       Console.print x"
  
                    --, "def main self:"
                    --, "    Console.print (self.add 3 4)"
                    --, "    Console.print (self.add \"Hello\" \" world!\")"
  
  
                    --, "    v = Vector 0 0 0"
                    --, "    Console.print v"
                    --, "    Console.print v"
                      --, "def main self:"
                      --, "    a = x:x"
                      ----, "    a = {a,a,a}"
                      --, "    a 5"
                    ]

--example :: Source
--example = Source.Source ["Main"]
--        $ unlines [ ""

--                  , "class Vector a:"
--                  , "    x,y,z :: a"
--                  ]


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
    (ast, srcMap) <- hoistEither =<< TxtParser.run source
    logger info "\n>> AST"
    logger info $ PP.ppqShow ast
    logger info "\n>> Source Map"
    logger info $ PP.ppShow srcMap
    --logger info (show.length $ FModule._classes ast)
    return ()

    --let crumbs = [ASTCrumb.ModuleCrumb "Main", ASTCrumb.FunctionCrumb "add"]

    --let ast2 =     Zipper.mk ast
    --           >>= Zipper.focusFunction "add"
    --           >>= Zipper.modify (\(Zipper.FunctionFocus func) -> Zipper.FunctionFocus (func & LExpr.name .~ "dupa"))
    --           >>= Zipper.close

    --logger info $ PP.ppqShow ast2

    --putStrLn $ PP.ppShow zipper

    logger info "\n-------- VarAlias --------"
    va <- hoistEither =<< VarAlias.run     ast
    logger info $ PP.ppShow va

    logger info "\n-------- FuncPool --------"
    fp <- hoistEither =<< FuncPool.run ast
    logger info $ PP.ppShow fp

    logger info "\n-------- Hash --------"
    hash <- hoistEither =<< Hash.run ast
    logger info $ PP.ppShow hash

    logger info "\n-------- SSA --------"
    ssa <- hoistEither =<< SSA.run va hash

    --logger info $ PP.ppqShow ssa

    logger info "\n-------- HASTGen --------"
    hast <- hoistEither =<< HASTGen.run ssa fp
    logger info $ PP.ppShow hast

    logger info "\n-------- HSC --------"
    hsc <- hoistEither =<< HSC.run  hast
    logger info $ join "\n\n" (map printSrc hsc)


    return ()


main_graph :: IO (Either String ())
main_graph = Luna.run $ do
    let source  = example
        emptyPM = PropertyMap.empty

    logger info "\n-------- TxtParser --------"
    (ast, _) <- hoistEither =<< TxtParser.run source
    logger info $ PP.ppqShow ast

    logger info "\n-------- VarAlias --------"
    va <- hoistEither =<< VarAlias.runGather ast
    logger info $ PP.ppShow va

    (Focus.FunctionFocus expr) <- Zipper.mk ast
                              >>= Zipper.focusFunction "test"
                              >>= return . Zipper.getFocus

    logger info $ PP.ppShow expr
    (graph, pm) <- hoistEither =<< GraphBuilder.run va emptyPM expr
    let graphView = GraphView.fromGraph graph
        (graphWithDefaults, pmWithDefaults) = Defaults.addDefaults graphView pm
    logger warning $ show graph
    logger warning $ PP.ppShow pm
    --logger info $ show graphWithDefaults
    let (newGraphView, newPM) = Defaults.removeDefaults graphWithDefaults pmWithDefaults
    newGraph <- GraphView.toGraph newGraphView
    --logger warning $ show newGraph
    expr' <- hoistEither =<< GraphParser.run newGraph newPM expr
    logger info $ PP.ppShow expr'
    logger warning $ PP.ppShow newPM

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


