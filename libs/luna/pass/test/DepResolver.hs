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

import Flowbox.Prelude
import Data.HList (HList(HNil), (.*.))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

--import           Control.Applicative
--import           Control.Monad.RWS          hiding (join)
--import           Control.Monad.State        hiding (join)
--import           Control.Monad.Trans.Either
--import           Control.Monad.Trans.Maybe
--import           Control.Monad.Writer       hiding (join)
--import qualified Data.DList                 as DList
--import           Data.Either.Utils          (forceEither)
--import           Data.String.Utils          (join)
--import           Data.Version               (Version (Version))
--import           Debug.Trace
--import           System.TimeIt

--import           Flowbox.Prelude
--import qualified Flowbox.System.Log.LogEntry                                   as LogEntry
--import           Flowbox.System.Log.Logger
--import qualified Flowbox.System.Log.Logger                                     as Logger
--import qualified Flowbox.System.UniPath                                        as UniPath
--import           Flowbox.Text.Show.Hs                                          (hsShow)
--import qualified Flowbox.Text.Show.Pretty                                      as PP
--import qualified Luna.DEP.AST.Control.Crumb                                        as ASTCrumb
--import qualified Luna.DEP.AST.Control.Focus                                        as Focus
--import qualified Luna.DEP.AST.Control.Zipper                                       as Zipper
--import qualified Luna.DEP.AST.Expr                                                 as LExpr
--import qualified Luna.DEP.AST.Module                                               as FModule
--import qualified Luna.Data.AliasInfo                                           as AliasInfo
--import qualified Luna.Target.HS.AST.Expr                                           as HExpr
--import qualified Luna.Target.HS.AST.Module                                         as Module
--import           Luna.Data.Source                                              (Source)
--import qualified Luna.Data.Source                                              as Source
--import qualified Luna.Distribution.Cabal.Config                                as Config
--import qualified Luna.Distribution.Cabal.Section                               as Section
--import qualified Luna.Graph.PropertyMap                                        as PropertyMap
--import qualified Luna.Parser.Parser                                            as Parser
--import qualified Luna.Parser.Parser                                            as Parser
--import qualified Luna.Pass.Analysis.Alias.Alias                                as Analysis.Alias
--import qualified Luna.Pass.Analysis.CallGraph.CallGraph                        as Analysis.CallGraph
--import qualified Luna.Pass.Analysis.FuncPool.FuncPool                          as FuncPool
--import qualified Luna.Pass.Analysis.ID.MaxID                                   as MaxID
--import qualified Luna.Pass.CodeGen.HSC.HSC                                     as HSC
--import qualified Luna.Pass.General.Luna.Luna                                   as Luna
--import qualified Luna.Pass.Source.File.Reader                                  as FileReader
--import qualified Luna.Pass.Transform.AST.DepSort.DepSort                       as Transform.DepSort
--import qualified Luna.Pass.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
--import qualified Luna.Pass.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
--import qualified Luna.Pass.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
--import qualified Luna.Pass.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt
--import qualified Luna.Pass.Transform.AST.Hash.Hash                             as Hash
--import qualified Luna.Pass.Transform.AST.SSA.SSA                               as SSA
--import qualified Luna.Pass.Transform.AST.TxtParser.TxtParser                   as TxtParser
--import qualified Luna.Pass.Transform.HAST.HASTGen.HASTGen                      as HASTGen


import           Luna.Pass.Pass (Pass(Pass))
import qualified Luna.Pass.Pass as Pass
import           Control.Monad.Trans.Either
import           Control.Error.Util (note)

import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Graph.Inductive.Query.DFS as DFS
import Data.Dynamic

import qualified Data.Set as Set

import Data.Graph.Inductive.Graph (insNode, insEdge, Edge, UEdge)
import Data.Graph.Inductive.PatriciaTree

import Data.Maybe (fromJust)

type PassID = Int

type DataReq = [Pass.Data]

data Step = Run { _id     :: PassID
                , _needed :: DataReq
                } deriving (Show)


data PassEntry a = PassEntry { _passid :: PassID
                             , _pass   :: (Pass a)
                             } deriving (Show)


type DepGraph = Gr DepNode ()


--data Graph = Graph [DepNode]
--           deriving (Show)

data DepNode = PassNode String
             | DataNode (Pass.Data)
             | SCCGraph DepGraph
             deriving (Show)

makeLenses ''PassEntry

p1 = Pass [] ["width"]  foo1
p2 = Pass [] ["height"] foo2
p3 = Pass ["width", "height"] ["area"] foo3
passes = p1 .*. p2 .*. p3 .*. HNil
--providers = view Pass.inputs p3

registerPassEntry (PassEntry id p) map = foldr insert map items where
    insert    = uncurry $ Map.insertWith (IntSet.union)
    items     = zip outputs $ repeat $ IntSet.singleton id
    outputs   = view Pass.outputs p

providers = registerPassEntry (PassEntry 0 p1)
          $ registerPassEntry (PassEntry 1 p2)
          $ registerPassEntry (PassEntry 2 p3)
          $ Map.empty

labUEdges :: [Edge] -> [UEdge]
labUEdges = map (\(i,j) -> (i,j,()))

node = (,)
edge = (,,())


noEdges = []



g :: DepGraph
g = insEdge (edge 1 2)
  $ insEdge (edge 2 3)
  $ insEdge (edge 3 4)
  $ insEdge (edge 3 6)
  $ insEdge (edge 3 7)
  $ insEdge (edge 4 5)
  $ insEdge (edge 5 6)
  $ insEdge (edge 6 3)
  $ insNode (node 0 $ PassNode "File Reader")
  $ insNode (node 1 $ DataNode "src")
  $ insNode (node 2 $ PassNode "Parser stage 1")
  $ insNode (node 3 $ DataNode "ast")
  $ insNode (node 4 $ PassNode "Alias Analysis")
  $ insNode (node 5 $ DataNode "aa")
  $ insNode (node 6 $ PassNode "Parser stage 2")
  $ insNode (node 7 $ PassNode "Code generation")
  $ Graph.empty

--c    = mkGraph (zip [1..3] "abc") (labUEdges [(1,3)]) :: Gr Char ()

main = do
    print g
    let scc = DFS.scc g
    print $ (fmap) (subgraph g) scc
    --print $ zip [0..] scc

    print "end"


idToNode g id = fromJust $ Graph.lab g id

sccToNode :: Int -> DepGraph -> [Int] -> (Int, DepNode)
sccToNode id g [x] = (x, fromJust $ Graph.lab g x)
sccToNode id g a   = (id, SCCGraph $ subgraph g a)

subgraph g nodes = Graph.delNodes overNodes g
    where overNodes = Set.toList $ Set.difference (Set.fromList $ Graph.nodes g) (Set.fromList nodes)

--(zip providers $ repeat p3)
--resolve :: DataReq -> Either String [Step]
resolve reqs = sequence $ fmap (\req -> note req $ flip Map.lookup providers req) reqs

foo1 = 5
foo2 = 6
foo3 a b = a + b
--genProject :: String -> Config.Config
--genProject name = let
--    exec = Section.mkExecutable name
--    conf = Config.addSection exec
--         $ Config.make name (Version [1] [])

--    in conf

----main_inner :: IO (Either String ())
----main_inner = Luna.run $ do
----    let conf = genProject
----    putStrLn $ Config.genCode conf

----    return ()




--logger :: LoggerIO
--logger = getLoggerIO $moduleName


----example :: Source
----example = Source.Source ["Std", "Console"]
----        $ unlines [ ""
----                  , "class Console:"
----                  , "    def print self msg:"
----                  , "        ```print #{msg}```"
----                  ]

--example :: Source
--example = Source.Source ["Main"] $
--        concat $ replicate 1 $ unlines [ ""
--                    --, "@AllowOrphans"
--                    , "def print msg:"
--                    , "    ```polyJoin . liftF1 (Value . fmap Safe . print) $ #{msg}```"
--                    , "def Int.> a:"
--                    , "    ```liftF2 (>) #{self} #{a}```"

--                    ----, "class Vector:"
--                    ----, "    x,y,z :: [Vector]"
--                    --, "def Int.+ a:"
--                    --, "   a"
--                    --, "class ```Maybe``` a:"
--                    --, "    Just: unwrap :: a"
--                    --, "    Nothing"
--                    --, "def foo a b:"
--                    --, "    print a"
--                    --, "    print b"
--                    --, "def > a b:"
--                    --, "    a.> b"

--                    , "def main:"
--                    --, "    a = Vector [] [] []"
--                    --, "    print [1..5]"
--                    --, "    foo 1 2"
--                    , "    print $ 1"
--                    ]


--        --concat $ replicate 1 $ unlines [ ""
--        --            --, "@AllowOrphans"
--        --            , "class Vector a:"
--        --            , "    Vector: x,y,z :: a"
--        --            , "    Scalar: w     :: a"

--        --            , "def print msg:"
--        --            , "    ```polyJoin . liftF1 (Value . fmap Safe . print) $ #{msg}```"


--        --            , "def main:"
--        --            , "    v = Vector 1 2 3"
--        --            , "    s = Scalar 5"
--        --            , "    Vector a b c = v"
--        --            , "    print a"
--        --            ]


--main :: IO ()
--main = do
--    --DistMain.main
--    Logger.setLevel DEBUG ""
--    --let x = Parser.parse' example
--    --    --x :: Int

--    --print $ x

--    out <- timeIt main_inner
--    case out of
--        Right _ -> return ()
--        Left  e -> putStrLn e

--    --out <- timeIt main_graph
--    --case out of
--    --    Right _ -> return ()
--    --    Left  e -> putStrLn e



--main_inner :: IO (Either String ())
--main_inner = Luna.run $ do
--    let source = example

--    logger info "\n-------- TxtParser --------"
--    (ast, srcMap, astInfo) <- hoistEither =<< TxtParser.run source
--    logger info "\n>> AST"
--    logger info $ PP.ppqShow ast
--    logger info "\n>> Source Map"
--    logger info $ PP.ppShow srcMap
--    logger info "\n>> AST Info"
--    logger info $ PP.ppShow astInfo
--    --logger info (show.length $ FModule._classes ast)
--    return ()

--    --let crumbs = [ASTCrumb.ModuleCrumb "Main", ASTCrumb.FunctionCrumb "add"]

--    --let ast2 =     Zipper.mk ast
--    --           >>= Zipper.focusFunction "add"
--    --           >>= Zipper.modify (\(Zipper.FunctionFocus func) -> Zipper.FunctionFocus (func & LExpr.name .~ "dupa"))
--    --           >>= Zipper.close

--    --logger info $ PP.ppqShow ast2

--    --putStrLn $ PP.ppShow zipper


--    -- Should be run BEFORE Analysis.Alias
--    logger info "\n-------- Desugar.ImplicitSelf --------"
--    (ast, astInfo) <- hoistEither =<< Desugar.ImplicitSelf.run astInfo ast
--    logger info $ PP.ppqShow ast


--    logger info "\n-------- Desugar.TLRecUpdt --------"
--    (ast, astInfo) <- hoistEither =<< Desugar.TLRecUpdt.run astInfo ast
--    logger info $ PP.ppqShow ast


--    logger info "\n-------- Analysis.Alias --------"
--    aliasInfo <- hoistEither =<< Analysis.Alias.run ast
--    logger info "\n>> scope:"
--    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.scope)
--    logger info "\n>> alias map:"
--    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.alias)
--    logger info "\n>> parent map:"
--    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.parent)
--    logger info "\n>> orphans map:"
--    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.orphans)
--    --logger info "\n>> parentMap"
--    --logger info $ PP.ppShow (aliasInfo ^. AliasInfo.orphans)

--    -----------------------------------------
--    -- !!! CallGraph and DepSort are mockup passes !!!
--    -- They are working right now only with not self typed variables
--    logger info "\n-------- Analysis.CallGraph --------"
--    callGraph <- hoistEither =<< Analysis.CallGraph.run aliasInfo ast
--    logger info $ PP.ppShow callGraph


--    logger info "\n-------- Transform.DepSort --------"
--    ast <- hoistEither =<< Transform.DepSort.run callGraph aliasInfo ast
--    logger info $ PP.ppShow ast
--    -----------------------------------------


--    -- !!! [WARNING] INVALIDATES aliasInfo !!!
--    logger info "\n-------- Desugar.ImplicitScopes --------"
--    (ast, astInfo) <- hoistEither =<< Desugar.ImplicitScopes.run astInfo aliasInfo ast
--    logger info $ PP.ppqShow ast


--    -- Should be run AFTER ImplicitScopes
--    logger info "\n-------- Desugar.ImplicitCalls --------"
--    (ast, astInfo) <- hoistEither =<< Desugar.ImplicitCalls.run astInfo ast
--    logger info $ PP.ppqShow ast


--    logger info "\n-------- Analysis.Alias --------"
--    aliasInfo <- hoistEither =<< Analysis.Alias.run ast
--    logger info "\n>> scope:"
--    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.scope)
--    logger info "\n>> alias map:"
--    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.alias)
--    logger info "\n>> orphans map:"
--    logger info $ PP.ppShow (aliasInfo ^. AliasInfo.orphans)





--    --logger info "\n-------- FuncPool --------"
--    --fp <- hoistEither =<< FuncPool.run ast
--    --logger info $ PP.ppShow fp

--    logger info "\n-------- Hash --------"
--    hash <- hoistEither =<< Hash.run ast
--    --logger info $ PP.ppShow hash

--    logger info "\n-------- SSA --------"
--    ssa <- hoistEither =<< SSA.run aliasInfo hash
--    --logger info $ PP.ppqShow ssa

--    logger info "\n-------- HASTGen --------"
--    hast <- hoistEither =<< HASTGen.run ssa
--    logger info $ PP.ppShow hast

--    logger info "\n-------- HSC --------"
--    hsc <- hoistEither =<< HSC.run  hast
--    logger info $ join "\n\n" (map showSrc hsc)


--    return ()


--showSrc :: Source -> String
--showSrc src = ">>> file '" ++ join "/" (src ^. Source.path) ++ "':\n\n"
--             ++ hsShow (src ^. Source.code)
