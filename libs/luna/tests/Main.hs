---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}



import           Debug.Trace                                             
import           Data.Either.Utils                                       (forceEither)
import qualified Data.DList                                            as DList
import           Control.Applicative                                     
import           Control.Monad.State                                     
import           Control.Monad.Writer                                    
import           Control.Monad.RWS                                       
import           Control.Monad.Trans.Maybe                               
import           Control.Monad.Trans.Either                              
import           System.TimeIt                                           

import           Flowbox.Prelude                                         
import qualified Flowbox.Luna.Passes.Cabal.Build.CabalBuild            as CabalBuild
import qualified Flowbox.Luna.Passes.Cabal.Run.CabalRun                as CabalRun
import qualified Flowbox.Luna.Passes.Cabal.Gen.CabalGen                as CabalGen
import qualified Flowbox.Luna.Passes.Cabal.Store.CabalStore            as CabalStore
import qualified Flowbox.Luna.Passes.Transform.Source.Reader.Reader    as SourceReader
import qualified Flowbox.Luna.Data.HAST.Expr                           as Expr
import qualified Flowbox.Luna.Data.HAST.Module                         as Module
import qualified Flowbox.Luna.Passes.Transform.HS.HASTGen.HASTGen      as HASTGen
import qualified Flowbox.Luna.Passes.Transform.HS.CodeGen.CodeGen      as CodeGen
import qualified Flowbox.Luna.Passes.Transform.HS.Print.Print          as HSPrint
import qualified Flowbox.Luna.Passes.General.Luna.Luna                 as Luna
import qualified Flowbox.Luna.Passes.Transform.SSA.SSA                 as SSA
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser as TxtParser
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias        as VarAlias
import qualified Flowbox.Luna.Data.Source                              as Source
import           Flowbox.Luna.Data.Source                                (Source)
import           Flowbox.System.Log.Logger                               
import qualified Flowbox.System.Log.Logger                             as Logger
import qualified Flowbox.System.Log.LogEntry                           as LogEntry
import qualified Flowbox.System.UniPath                                as UniPath
import qualified Flowbox.Text.Show.Pretty                              as PP



logger :: Logger
logger = getLogger "Flowbox"


example :: Source
example = Source.Source "Workspace"
        $ unlines [ ""
                  , "def f x y: "
                  , "    x.add y"
                  ]


main :: IO ()
main = do
    logger setLevel DEBUG
    
    out <- timeIt main_inner
    case out of
        Right _ -> return ()
        Left  e -> putStrLn e


main_inner :: IO (Either String ())
main_inner = Luna.run $ do

    cabal <- CabalGen.run "TestProject2"

    CabalStore.run cabal $ UniPath.fromUnixString "samples/TestProject2/build/hs/TestProject2.cabal"
    CabalBuild.run $ UniPath.fromUnixString "samples/TestProject2"
    CabalRun.run (UniPath.fromUnixString "samples/TestProject2") "TestProject2" []
    --source <- SourceReader.run (UniPath.fromUnixString "samples/TestProject2/src")
    --                           (UniPath.fromUnixString "samples/TestProject2/src/Workspace/Main.luna")
                               
    let source = example
    putStrLn "\n-------- TxtParser --------"
    ast <- TxtParser.run source
    putStrLn $ PP.ppqShow ast

    putStrLn "\n-------- VarAlias --------"
    va <- VarAlias.run     ast
    putStrLn $ PP.ppShow va

    putStrLn "\n-------- SSA --------" 
    ssa <- SSA.run va ast
    putStrLn $ PP.ppqShow ssa

    putStrLn "\n-------- HASTGen --------" 
    hast <- HASTGen.run  ssa
    putStrLn $ PP.ppShow hast

    --putStrLn "\n-------- HS CodeGen --------" 
    hsc <- CodeGen.run  hast
    --putStrLn $ hsc

    putStrLn "\n-------- PHSC --------" 
    phsc <- HSPrint.run hsc
    putStrLn $ phsc

    return ()





