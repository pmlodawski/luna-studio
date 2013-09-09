---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


import           Flowbox.Prelude                           

import           Control.Monad.State                       
import           Control.Monad.Writer                      
import           Control.Monad.RWS                         
import           Control.Monad.Trans.Maybe                 
import           Control.Monad.Trans.Either                
import           Flowbox.System.Log.Logger                 
import qualified Flowbox.System.Log.Logger               as Logger
import qualified Flowbox.System.Log.LogEntry             as LogEntry
import qualified Flowbox.Luna.Passes.HSGen.HSGen         as HSGen
import qualified Flowbox.Luna.Passes.SSA.SSA             as SSA
import qualified Flowbox.Luna.Passes.HSGen.AST.Module    as Module
import qualified Flowbox.Luna.Passes.HSGen.AST.Expr      as Expr
import qualified Flowbox.Luna.Passes.Graph2AST.Graph2AST as Graph2AST
import qualified Flowbox.Luna.Passes.SSA.State           as SSAState
import           Flowbox.Luna.Passes.SSA.State             (SSAState)
import qualified Flowbox.Luna.Passes.Luna.Luna           as Luna
import qualified Flowbox.Luna.Passes.Txt2AST.Txt2AST     as Txt2AST
import qualified Flowbox.Luna.Data.Source                as Source
import           Flowbox.Luna.Data.Source                  (Source)

import           Debug.Trace                               
import           Data.Either.Utils                         (forceEither)
import qualified Text.Show.Pretty                        as PP
import qualified Data.DList                              as DList

import           Control.Applicative                       

import           System.TimeIt                             

logger :: Logger
logger = getLogger "Flowbox"


example :: Source
example = Source.Source "Workspace"
        $ unlines [ "import Std.Math.Vector as V"
                  ]


main :: IO ()
main = do
    out <- timeIt main_inner
    case out of
        Right _ -> return ()
        Left  e -> putStrLn e

main_inner :: IO (Either String ())
main_inner = Luna.run $ do
    putStrLn "\n-------- AST --------"
    ast <- Txt2AST.run example
    putStrLn $ PP.ppShow ast

    putStrLn "\n-------- SSA --------"
    ssa <- SSA.run     ast
    putStrLn $ PP.ppShow ssa

    putStrLn "\n-------- HAST --------" 
    hast <- HSGen.run  ssa
    putStrLn $ PP.ppShow hast


    --putStrLn "\n-------- HSC --------"
    --hsc <- HSGen.genCode  hast
    --putStrLn $ PP.ppShow hsc

    return ()



