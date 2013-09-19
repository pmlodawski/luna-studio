---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


import           Debug.Trace                                   
import           Data.Either.Utils                             (forceEither)
import qualified Data.DList                                  as DList
import           Control.Applicative                           
import           Control.Monad.State                           
import           Control.Monad.Writer                          
import           Control.Monad.RWS                             
import           Control.Monad.Trans.Maybe                     
import           Control.Monad.Trans.Either                    
import           System.TimeIt                                 

import           Flowbox.Prelude                               
import qualified Flowbox.Luna.Passes.FileReader.SourceReader as SourceReader
import qualified Flowbox.Luna.Passes.HSGen.AST.Expr          as Expr
import qualified Flowbox.Luna.Passes.HSGen.AST.Module        as Module
import qualified Flowbox.Luna.Passes.HSGen.HSC               as HSC
import qualified Flowbox.Luna.Passes.HSGen.HSGen             as HSGen
import qualified Flowbox.Luna.Passes.HSPrint.HSPrint         as HSPrint
import qualified Flowbox.Luna.Passes.Luna.Luna               as Luna
import qualified Flowbox.Luna.Passes.SSA.SSA                 as SSA
import qualified Flowbox.Luna.Passes.Txt2AST.Txt2AST         as Txt2AST
import qualified Flowbox.Luna.Passes.VA.VA                   as VA
--import qualified Flowbox.Luna.Passes.SSA.State           as SSAState
--import           Flowbox.Luna.Passes.SSA.State             (SSAState)
import qualified Flowbox.Luna.Data.Source                    as Source
import           Flowbox.Luna.Data.Source                      (Source)
import           Flowbox.System.Log.Logger                     
import qualified Flowbox.System.Log.Logger                   as Logger
import qualified Flowbox.System.Log.LogEntry                 as LogEntry
import qualified Flowbox.System.UniPath                      as UniPath
import qualified Flowbox.Text.Show.Pretty                    as PP



logger :: Logger
logger = getLogger "Flowbox"


example :: Source
example = Source.Source "Workspace"
        $ unlines [ "def f x y:"
                  , "    Std.Math.add x y"
                  ]


--example :: Source
--example = Source.Source "Workspace"
--        $ unlines [ "class A:"
--                  , "    a :: Std.Math.Vector (Int a) b"
--                  , "    a :: Std.Math.Vector (Int a) b"
--                  ]

--example :: Source
--example = Source.Source "Workspace"
--        $ unlines [ "def f (a::Vector a b Int c):"
--                  , "    a = Vector a b Int c"
--                  ]


main :: IO ()
main = do
    out <- timeIt main_inner
    case out of
        Right _ -> return ()
        Left  e -> putStrLn e


main_inner :: IO (Either String ())
main_inner = Luna.run $ do
    --source <- SourceReader.run (UniPath.fromUnixString "samples/TestProject2/src")
    --                           (UniPath.fromUnixString "samples/TestProject2/src/Workspace/Main.luna")
                               
    let source = example
    putStrLn "\n-------- Txt2AST --------"
    ast <- Txt2AST.run source
    putStrLn $ PP.ppqShow ast

    --putStrLn "\n-------- VA --------"
    --va <- VA.run     ast
    --putStrLn $ PP.ppShow va

    --putStrLn "\n-------- SSA --------" 
    --ssa <- SSA.run va ast
    --putStrLn $ PP.ppqShow ssa

    --putStrLn "\n-------- HSGen --------" 
    --hast <- HSGen.run  ssa
    --putStrLn $ PP.ppShow hast

    ----putStrLn "\n-------- HSC --------" 
    --hsc <- HSC.run  hast
    ----putStrLn $ hsc

    --putStrLn "\n-------- PHSC --------" 
    --phsc <- HSPrint.run hsc
    --putStrLn $ phsc

    return ()

