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
import           Control.Monad.State                                   hiding (join)
import           Control.Monad.Writer                                  hiding (join)
import           Control.Monad.RWS                                     hiding (join)
import           Control.Monad.Trans.Maybe                               
import           Control.Monad.Trans.Either                              
import           System.TimeIt                                           

import           Flowbox.Prelude                                         
import qualified Flowbox.Luna.Passes.Source.File.Reader                as FileReader
import qualified Flowbox.Luna.Data.HAST.Expr                           as Expr
import qualified Flowbox.Luna.Data.HAST.Module                         as Module
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen    as HASTGen
import qualified Flowbox.Luna.Passes.CodeGen.HSC.HSC                   as HSC
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
import           Data.String.Utils                                       (join)
import           Flowbox.Text.Show.Hs                                    (hsShow)


import qualified Flowbox.Luna.Data.Cabal.Config                        as Config
import qualified Flowbox.Luna.Data.Cabal.Section                       as Section


genProject :: String -> Config.Config
genProject name = let
    exec = Section.mkExecutable name
    conf = Config.addSection exec
         $ Config.make name

    in conf

--main_inner :: IO (Either String ())
--main_inner = Luna.run $ do
--    let conf = genProject
--    putStrLn $ Config.genCode conf

--    return ()


logger :: Logger
logger = getLogger "Flowbox"


example :: Source
example = Source.Source ["Main"]
        $ unlines [ ""
                  , "class Console:"
                  , "    def print self msg:"
                  , "        ```print #{msg}```"
                  , "class Vector a:"
                  , "    x,y,z :: a"
                  , "    def vtest self:"
                  , "        {1,2,3}"
                  , "def main self:"
                  , "    v = Vector 1 2 3"
                  , "    Console.print (v.vtest)"

                  --, "    v = Vector 0 0 0"
                  --, "    Console.print v"
                  --, "    Console.print v"
                  ]

--example :: Source
--example = Source.Source ["Main"]
--        $ unlines [ ""
--                  , "class Vector a:"
--                  , "    x,y,z :: a"
--                  ]


main :: IO ()
main = do
    logger setLevel DEBUG
    
    out <- timeIt main_inner
    case out of
        Right _ -> return ()
        Left  e -> putStrLn e


main_inner :: IO (Either String ())
main_inner = Luna.run $ do
    let source = example

    logger info "\n-------- TxtParser --------"
    ast <- TxtParser.run source
    logger info $ PP.ppqShow ast 

    logger info "\n-------- VarAlias --------"
    va <- VarAlias.run     ast
    logger info $ PP.ppShow va

    logger info "\n-------- SSA --------" 
    ssa <- SSA.run va ast
    logger info $ PP.ppqShow ssa

    logger info "\n-------- HASTGen --------" 
    hast <- HASTGen.run  ssa
    logger info $ PP.ppShow hast

    logger info "\n-------- HSC --------" 
    hsc <- HSC.run  hast
    logger info $ join "\n\n" (map printSrc hsc)


    return ()


printSrc :: Source -> [Char]
printSrc src = ">>> file '" ++ join "/" (Source.path src) ++ "':\n\n"
             ++ hsShow (Source.code src)


