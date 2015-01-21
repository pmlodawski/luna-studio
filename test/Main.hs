{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Main where


-- <<<<<<< HEAD
import            Luna.Data.Namespace                       (Namespace (Namespace))
import            Luna.Data.Source                          (Text (Text), Source (Source))
-- =======
-- import qualified Luna.Pass as Pass
-- import qualified Luna.Pass.Transform.Parse.Stage2 as Stage2
-- import qualified Luna.Pass.Transform.Parse.Stage1 as Stage1
-- import           Luna.Data.Namespace (Namespace(Namespace))
-- import Luna.Data.Source (Source(Source), Medium(String), Code(Code))
-- import qualified Luna.Pass.Analysis.Alias as AA
-- 
-- 
-- import Control.Monad.Trans.Either
-- import Control.Applicative
-- import Control.Monad
-- 
-- import Control.Lens hiding (without)
-- import Data.List (intercalate)
-- >>>>>>> feature/ast2

import qualified  Luna.Pass                                 as Pass
--import qualified  Luna.Pass.Analysis.Struct                 as P2SA
--import qualified  Luna.Pass2.Target.HS.HASTGen              as P2HASTGen
--import qualified  Luna.Pass2.Target.HS.HSC                  as P2HSC
--import qualified  Luna.Pass.Transform.Desugar.ImplicitSelf  as P2ImplSelf
--import qualified  Luna.Pass.Transform.Hash                  as P2Hash
--import qualified  Luna.Pass.Transform.Parse.Stage1          as P2Stage1
--import qualified  Luna.Pass.Transform.Parse.Stage2          as P2Stage2
--import qualified  Luna.Pass.Transform.SSA                   as P2SSA

import            Control.Monad.Trans.Either
import            Data.Text.Lazy                            (pack,unpack)

import            System.Environment                        (getArgs)
import            Text.Show.Pretty                          (ppShow)

import qualified  Luna.Typechecker.Inference                as PTyChk
import            Luna.Typechecker.Debug.ConsoleColours     (PrintAttrs(..),colouredPrint,writeFileM)


import qualified  Luna.System.Session as Session
import qualified  Luna.Parser.Parser as Parser

import qualified Luna.Pass.Transform.Parse.Stage2 as Stage2
import qualified Luna.Pass.Transform.Parse.Stage1 as Stage1
import qualified Luna.Pass.Transform.Desugar.ImplicitSelf as ImplSelf
import qualified Luna.Pass.Transform.SSA                  as SSA
import qualified Luna.Pass.Transform.Desugar.ImplicitScopes as ImplScopes
import qualified Luna.Pass.Transform.Desugar.ImplicitCalls as ImplCalls
import qualified Luna.Pass.Analysis.Struct as SA
import qualified Luna.Pass.Target.HS.HASTGen              as HASTGen
import qualified Luna.Pass.Target.HS.HSC                  as HSC
import Flowbox.Text.Show.Hs (hsShow)

import System.Exit
import Control.Monad.IO.Class

main :: IO ()
main = do
  args <- getArgs
  case args of
      []      -> [Red, Bold] `colouredPrint` "no file given, sorry"
      (x:y:_) -> [Red, Bold] `colouredPrint` "too many args given - requires one (path to *.luna file). Sorry"
      [file]  -> do
        [Bold,Green]  `colouredPrint` file
        [Cyan]        `colouredPrint` "…reading `" ++ file ++ "`"
        file_contents <- do tmp <- readFile file
                            tmp `seq` return tmp
        [Cyan] `colouredPrint` "…passes"

        let src = Source (pack file) (Text $ pack file_contents)

        Session.runT $ do
          Parser.init
          result <- runEitherT $ do

            (ast1, astinfo1) <- Pass.run1_ Stage1.pass src
            writeFileM              " 1.1. Stage1     : ast1"        $ ppShow ast1
            writeFileM              " 1.2. Stage1     : astinfo1"    $ ppShow astinfo1

            sa2              <- Pass.run1_ SA.pass ast1
            writeFileM              " 2.   SA         : sa2"         $ ppShow sa2
        
            (ast3, astinfo3) <- Pass.run3_ Stage2.pass (Namespace [] sa2) astinfo1 ast1
            writeFileM              " 3.1. Stage2     : ast3"        $ ppShow ast3
            writeFileM              " 3.2. Stage2     : astinfo3"    $ ppShow astinfo3
        
            (ast4, astinfo4) <- Pass.run2_ ImplSelf.pass astinfo3 ast3
            writeFileM              " 4.1. ImplSelf   : ast4"        $ ppShow ast4
            writeFileM              " 4.2. ImplSelf   : astinfo4"    $ ppShow astinfo4
        
            sa5              <- Pass.run1_ SA.pass ast4
            writeFileM              " 5.   SA         : sa5"         $ ppShow sa5

            -- TODO [kgdk] 20 sty 2015: przenieść do tests
            -- TODO [kgdk] 19 sty 2015: dopisać typy
            constraints      <- Pass.run2_ PTyChk.tcpass ast4 sa5
            writeFileM              " 6.   PTyChk     : constraints" $ ppShow constraints
        
            (ast6, astinfo6) <- Pass.run3_ ImplScopes.pass astinfo4 sa5 ast4
            writeFileM              " 7.1. ImplScopes : ast6"        $ ppShow ast6
            writeFileM              " 7.2. ImplScopes : astinfo6"    $ ppShow astinfo6

            (ast7, astinfo7) <- Pass.run2_ ImplCalls.pass astinfo6 ast6
            writeFileM              " 8.1. ImplCalls  : ast7"        $ ppShow ast7
            writeFileM              " 8.2. ImplCalls  : astinfo7"    $ ppShow astinfo7

            ast8             <- Pass.run1_ SSA.pass ast7
            writeFileM              " 9.   SSA        : ast8"        $ ppShow ast8
            
            hast9            <- Pass.run1_ HASTGen.pass ast8
            writeFileM              "10.   HAST       : hast9"       $ ppShow hast9

            hsc10            <- Pass.run1_ HSC.pass hast9
            writeFileM              "11.   HSC        : hsc10"       $ hsShow $ unpack hsc10

            return ()

          case result of
            Left _   -> do
                [Red, Bold] `colouredPrint` "some error, sorry"
                liftIO $ exitWith (ExitFailure 1)
            Right () -> return ()

        return ()

