---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


import           Control.Monad.State                    
import           Control.Monad.Writer                   
import           Control.Monad.RWS                      
import           Control.Monad.Trans.Maybe              
import           Control.Monad.Trans.Either             
import           Flowbox.System.Log.Logger              
import qualified Flowbox.System.Log.Logger            as Logger
import qualified Flowbox.System.Log.LogEntry          as LogEntry
import qualified Flowbox.Luna.Passes.HSGen.Generator  as Gen
import qualified Flowbox.Luna.Passes.SSA.SSA          as SSA
import qualified Flowbox.Luna.Passes.HSGen.AST.Module as Module
--import qualified Flowbox.Luna.Parser                  as Parser
import qualified Flowbox.Luna.Passes.HSGen.AST.Expr   as Expr
import qualified Flowbox.Luna.Passes.SSA.State        as SSAState
import           Flowbox.Luna.Passes.SSA.State          (SSAState)
import qualified Flowbox.Luna.Passes.Luna.Luna        as Luna
import qualified Flowbox.Luna.Passes.Parser.Parser    as Parser

import           Debug.Trace                            
import           Data.Either.Utils                      (forceEither)
import qualified Text.Show.Pretty                     as PP
import qualified Data.DList                           as DList

import           Prelude                              hiding (log)
import           System.TimeIt                          


logger = getLogger "Flowbox"


example :: String
example = unlines [ "def f(x):"
                  , "   x=y+1"
                  , "   x=x x"
                  ]


main :: IO ()
main = timeIt main_inner

main_inner :: IO ()
main_inner = do
    out <- Luna.run $ do
        ast <- Parser.run example
        --ssa <- SSA.run ast
        return ast

    --let 
    --    parsed = Parser.parse example
    --    ast = forceEither parsed
    --    (nast, nstate, nlog) = SSA.run ast
    --    aaaa = test
    --    --(hast, hstate, hlog) = Gen.run nast
    ----let y = runRWS (runMaybeT test) 0 0
    --putStrLn $ PP.ppShow $ ast
    --putStrLn "\n-----------------"
    --putStrLn $ PP.ppShow $ nast
    --putStrLn $ PP.ppShow $ nstate
    --putStrLn $ PP.ppShow $ DList.toList nlog
    --putStrLn "\n-----------------"

    ----case nast of
    ----    Just nast' -> putStrLn $ PP.ppShow $ map Expr.genCode nast'
    ----    _          -> return ()
    print out
    return ()

