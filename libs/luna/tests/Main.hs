---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import           Control.Applicative                                     
import           Control.Monad.RWS                                     hiding (join)
import           Control.Monad.State                                   hiding (join)
import           Control.Monad.Trans.Either                              
import           Control.Monad.Trans.Maybe                               
import           Control.Monad.Writer                                  hiding (join)
import           Data.Either.Utils                                       (forceEither)
import qualified Data.DList                                            as DList
import           Data.String.Utils                                       (join)
import           Debug.Trace                                             
import           System.TimeIt                                           

import           Flowbox.Prelude                                         
import qualified Flowbox.Luna.Passes.Source.File.Reader                as FileReader
import           Data.Version                                            (Version(Version))
import qualified Flowbox.Luna.Data.Cabal.Config                        as Config
import qualified Flowbox.Luna.Data.Cabal.Section                       as Section
import qualified Flowbox.Luna.Data.HAST.Expr                           as HExpr
import qualified Flowbox.Luna.Data.AST.Expr                            as LExpr
import qualified Flowbox.Luna.Data.HAST.Module                         as Module
import qualified Flowbox.Luna.Data.Source                              as Source
import           Flowbox.Luna.Data.Source                                (Source)
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen    as HASTGen
import qualified Flowbox.Luna.Passes.CodeGen.HSC.HSC                   as HSC
import qualified Flowbox.Luna.Passes.General.Luna.Luna                 as Luna
import qualified Flowbox.Luna.Passes.Transform.SSA.SSA                 as SSA
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser as TxtParser
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias        as VarAlias
import qualified Flowbox.Luna.Passes.Analysis.FuncPool.FuncPool        as FuncPool
import           Flowbox.System.Log.Logger                               
import qualified Flowbox.System.Log.Logger                             as Logger
import qualified Flowbox.System.Log.LogEntry                           as LogEntry
import qualified Flowbox.System.UniPath                                as UniPath
import           Flowbox.Text.Show.Hs                                    (hsShow)
import qualified Flowbox.Text.Show.Pretty                              as PP

import qualified Flowbox.Luna.Data.AST.Crumb.Crumb                     as ASTCrumb
import qualified Flowbox.Luna.Data.AST.Zipper                          as Zipper

import           Control.Lens                                          hiding (Zipper)

import qualified Flowbox.Distribution.M                                as DistMain

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




logger :: Logger
logger = getLogger "Flowbox"


--example :: Source
--example = Source.Source ["Std", "Console"]
--        $ unlines [ ""
--                  , "class Console:"
--                  , "    def print self msg:"
--                  , "        ```print #{msg}```"
--                  ]

example :: Source
example = Source.Source ["Main"]
        $ unlines [ ""
                  --, "def List.length self:"
                  --, "    ```getIO $ liftFPure1 length #{self}```"
                  --, "def List.each self callback:"
                  --, "    ```let {mymap x (Pure y) = mapM x y}```"
                  --, "    ```getIO $ mymap (get1 #{callback}) #{self}```"
                  --, "def Int.add a b:"
                  --, "    ```getIO $ liftFPure2 (+) #{a} #{b}```"
                  --, "def Int.sub a b:"
                  --, "    ```getIO $ liftFPure2 (-) #{a} #{b}```"
                  --, "def Int.mul a b:"
                  --, "    ```getIO $ liftFPure2 (*) #{a} #{b}```"
                  --, "def List.add self x:"
                  --, "    ```getIO $ liftFPure2 (++) #{self} #{x}```"
                  --, "class Console:"
                  --, "    def print self msg:"
                  --, "        ```print #{msg}```"


                  --, "class Vector a:"
                  --, "    x,y,z :: a"
                  --, "def Vector.vtest self a b:"
                  --, "    {a,b}"       
                  --, "def test self a b:"
                  --, "    a + b"           
                  --, "    a = x: x.add 5"
                  --, "    v = Vector 1 2 3"
                  --, "    Console.print ([1,2,30..0].length)"

                  --, "    Console.print (1.add 2)"

                  --, "def main self:"
                  ----, "    Console.print {1}"
                  --, "    x = [1,2..30]"
                  --, "    y = x.each el:"
                  --, "        Console.print el"
                  --, "        el.add 1"
                  --, "    Console.print \"hello\""



                  , "def add self x y:"
                  , "   x.add y"

                  , "def add2 self x y:"
                  , "   x.add y"
                  --, "def addInts(self, x, y) -> Int:"
                  --, "   x.add y"
                  --, "def main self:"
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
    DistMain.main
    --logger setLevel DEBUG
    
    --out <- timeIt main_inner
    --case out of
    --    Right _ -> return ()
    --    Left  e -> putStrLn e




main_inner :: IO (Either String ())
main_inner = Luna.run $ do
    let source = example

    logger info "\n-------- TxtParser --------"
    ast <- TxtParser.run source
    logger info $ PP.ppqShow ast 

    let crumbs = [ASTCrumb.ModuleCrumb "Main", ASTCrumb.FunctionCrumb "add"]

    let ast2 =     Zipper.mk ast
               >>= Zipper.focusFunction "add"
               >>= Zipper.modify (\(Zipper.FunctionFocus func) -> Zipper.FunctionFocus (func & LExpr.name .~ "dupa"))
               >>= Zipper.close

    logger info $ PP.ppqShow ast2

    --putStrLn $ PP.ppShow zipper

    --logger info "\n-------- VarAlias --------"
    --va <- VarAlias.run     ast
    --logger info $ PP.ppShow va

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


