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
import qualified Flowbox.Luna.Passes.Analysis.FuncPool.FuncPool                          as FuncPool
import qualified Flowbox.Luna.Passes.Analysis.ID.MaxID                                   as MaxID
import qualified Flowbox.Luna.Passes.CodeGen.HSC.HSC                                     as HSC
import qualified Flowbox.Luna.Passes.General.Luna.Luna                                   as Luna
import qualified Flowbox.Luna.Passes.Source.File.Reader                                  as FileReader
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt
import qualified Flowbox.Luna.Passes.Transform.AST.Hash.Hash                             as Hash
import qualified Flowbox.Luna.Passes.Transform.AST.SSA.SSA                               as SSA
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser                      as Parser
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser                   as TxtParser
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.Builder                     as GraphBuilder
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.Parser                       as GraphParser
import qualified Flowbox.Luna.Passes.Transform.GraphView.Defaults.Defaults               as Defaults
import qualified Flowbox.Luna.Passes.Transform.GraphView.Defaults.Defaults               as Defaults
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen                      as HASTGen
import           Flowbox.Prelude
import qualified Flowbox.System.Log.LogEntry                                             as LogEntry
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Log.Logger                                               as Logger
import qualified Flowbox.System.UniPath                                                  as UniPath
import           Flowbox.Text.Show.Hs                                                    (hsShow)
import qualified Flowbox.Text.Show.Pretty                                                as PP

import qualified Flowbox.Luna.Data.AST.Crumb.Crumb as ASTCrumb

import Control.Lens hiding (Zipper)


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
                        --, "def List.each callback:"
                        --, "    ```liftf2 map (val $ call1 #{callback}) #{self}```"

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
                        --, "def List.sum self:"
                        --, "    ```liftf1 sum #{self}```"

                        --, "class Console:"
                        --, "import Std:All"
                        , "import ```Data.Accelerate```"
                        , "def print msg:"
                        , "    ```print' #{msg}```"

                        ----, "def Int.+ a b:"
                        ----, "    ```liftf2 (+) #{a} #{b}```"
                        ----, "class Vector a b c:"
                        ----, "    x :: a"
                        ------, "class Vector a = Vector | Scalar "
                        ------, "               | Scalar2"
                        ------, "class Vector a = Vector: x :: a"
                        ------, "                         y :: a"
                        ------, "                         z :: a"
                        ------, "               | Scalar: a :: a"
                        ------, "               | Vector2: x :: a"
                        ------, "               | Scalar"
                        ----, "class Vector a:"
                        ----, "    pos :: a"


                        ,"def Int.+ b:"
                        ,"    ```liftf2 (+) #{self} #{b}```"

                        --,"def Int.- b:"
                        --,"    ```liftf2 (-) #{self} #{b}```"

                        --,"def Int./ b:"
                        --,"    ```liftf2 (-) #{self} #{b}```"

                        --,"def Int.* b:"
                        --,"    ```liftf2 (*) #{self} #{b}```"

                        --,"def Int.< b:"
                        --,"    ```liftf2 (<) #{self} #{b}```"

                        ----, "class Point:"
                        ----, "    x,y,z :: Int"

                        --, "def raise el err:"
                        --, "    ```raise #{el} #{err}```"

                        --, "def catch el f:"
                        --, "    ```catch #{el} #{f}```"

                    , "def + x y: x+y"

                    , "def List.foldr f el:"
                    , "    ```flattenCtx $ (fmap.fmap) (foldr (call2 #{f}) #{el}) #{self}```"
                    
                    , "def List.sum:"
                    , "    self.foldr ((x,y):x+y) 0"

                    , "def List.sort:"
                    , "    #FIXME[wd]: very dirty hack for Pure Safe values."
                    , "    ```(fmap.fmap.fmap) val $ liftf1 sort ((fmap.fmap.fmap) (fromSafe.fromPure) #{self})```"

                    , "def List.head:"
                    , "    ```flattenCtx $ liftf1 head #{self}```"

                    , "def List.min:"
                    , "    self.sort.head"

                        --, "class X"
                        --, "    def test self:"
                        --, "        self"
                        --, "    def + self v2:"
                        --, "        Vector (self.x()+v2.x()) (self.y()+v2.y()) (self.z()+v2.z())"
                        ----, "    Scalar: a     :: a"
                        ----, "    def f self (x::Int):"

                    --, "    Console.print (1.add 2)"

                    --, "def f self a::Int b::Int :"
                    --, "    {a,b}"
                    --, "def f a::X :"
                    --, "    a"

                    --, "class X a:"
                    --, "    x :: a"
                    --, "    def test a:"
                    --, "        a + 1"

                    --, "def test2 x y: x"
                    --, "def +++ a b: a"

                    --, "def add x y: x.+y"

                    --, "def + x y: x.+ y"
                    ----, "def * x y: x.* y"
                    ----, "def / x y: x./ y"

                    ----, "def main:"
                    --, "class Vector a:"
                    --, "    x :: a"
 
                    ----, "    def + v:"
                    ----, "        Vector (x + v.x) (y + v.y) (z + v.z)"


                    --, "    def + v:"
                    --, "        print 111"
                    --, "        x + v.x"

                    --, "def Int.+ a:"
                    --, "    ```liftf2 (+) #{self} #{a}```"

                    --, "def Int.test:"
                    --, "    {self, self}"
                    
                    --, "def fun x:"
                    --, "    x.test"
                    
                    , "def main:"
                    --, "    v = Vector 1"
                    --, "    print (v + v)"
                    --, "    f = @(+)"
                    --, "    print $ (+) 1 2"
                    --, "    print $ @Int.+ 1 2"
                    , "    a = [1,5,3,4,2]"
                    , "    print a"
                    , "    print (a.sum)"
                    , "    print (a.sort)"
                    , "    print (a.min)"
                    --, "    print a.sort"
                    --, "    print $ a.foldr @(+) 0"
                    --, "    a = [1,2,3,4]"
                    --, "    a = 1"
                    --, "    f = @test"
                    --, "    x = X 1"
                    --, "    print $ x.test 1"
                    --, "    print $ @test2 1 2"
                    --, "    print $ @X.test x 2"
                    --, "    print $ (+++) 1 2"
                    --, "    print a"
                    --, "    print $ a.each x:"
                    --, "        x*2"
                    --, "    a = raise 1 (IOError \"Oh no\")"
                    --, "    catch a x:"
                    --, "        raise a x"
                    --, "    print a"
                    --, "    if 1<2:"
                    --, "        print 13"
                    --, "    else:"
                    --, "        print 15"
                    --, "    print (test 1 2)"
                    --, "    a = [1..10].each x:"
                    --, "        x * 2"
                    --, "    print a"
                    --, "    x = 1"
                    --, "    y = self.raise 2 (IOError \"Oh no\")"
                    --, "    z = x + y"
                    --, "    z = self.catch z x: 0"
                    --, "    c.print (x<2)"
                    --, "    a = if z<2:"
                    --, "            c.print 117"
                    ----, "        else:"
                    ----, "            0"
                    --, "    c.print a"
                    --, "    else:   4"
                    --, "    a = 1"
                    --, "    z.catch x"
                    --, "    a = self.test 5"
                    --, "    c.print (x<2)"
                    --, "    a = (f) 1"
                    --, "   c = Console()"
                    --, "   c.print $ self.f 5 6"
                    --, "   p = Point 1 2 3"
                    --, "   p.x = 0"
                    --, "   p.y = 0"
                    --, "   a_ = 10"
                    --, "   a = -10.0"

                    --, "alias PathW = String"
                    --, "type Path = String"
                    --, "   Point x y z = p"
                    --, "   c.print p"
                    --, "   v = Vector $ Point 1 2 3"
                    --, "   v.pos.x = v.pos.y = 0"
                    --, "   {a,b} = {x, z} = v"
                    --, "   c.print v"
                    --, "   c.print $ self.f 5 X()"

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


