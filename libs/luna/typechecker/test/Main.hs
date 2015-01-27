
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}





module Main where

import Prelude ()
import System.Environment (getArgs)
import Flowbox.Prelude
import qualified Luna.Parser.Parser as Parser
import           Text.PrettyPrint.ANSI.Leijen (displayIO, linebreak, renderPretty)
import Data.Default
import           System.IO                    (stdout)
import           Text.Show.Pretty
import qualified Luna.Parser.State as State
import qualified Luna.Parser.State as ParserState

import qualified Luna.Data.ASTInfo  as ASTInfo
import qualified Luna.Parser.Pragma as Pragma
import qualified Luna.Parser.Parser as Parser
import           Luna.Syntax.Name   (TName(TName))
import qualified Luna.Pass.Analysis.Struct as SA
import qualified Luna.Pass.Transform.Parse.Stage2 as Stage2
import qualified Luna.Pass.Transform.Parse.Stage1 as Stage1
import qualified Luna.Pass.Transform.Desugar.ImplicitSelf as ImplSelf
import qualified Luna.Pass.Transform.Hash                 as Hash
import qualified Luna.Pass.Transform.SSA                  as SSA
import qualified Luna.Pass.Target.HS.HASTGen              as HASTGen
import qualified Luna.Pass.Target.HS.HSC                  as HSC
import qualified Luna.Pass.Transform.Desugar.ImplicitScopes as ImplScopes
import qualified Luna.Pass.Transform.Desugar.ImplicitCalls as ImplCalls
import           Luna.Data.Namespace (Namespace(Namespace))
import qualified Luna.Pass as Pass
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as ByteStr
import Luna.Data.Source (Source(Source), File(File), Code(Code))
import qualified Luna.Data.Source as Source
import Data.Text.Lazy (unpack)
import           Flowbox.Text.Show.Hs                                          (hsShow)

import Luna.System.Session as Session
import qualified Luna.System.Pragma.Store as Pragma
import qualified Luna.Typechecker.Inference as TyChkr

header txt = "\n-------- " <> txt <> " --------"
printHeader = putStrLn . header

ppPrint = putStrLn . ppShow

main = do
    args <- getArgs
    let path = args !! 0
        src  = Source "Main" (File $ fromString path)

    Session.runT $ do

        Parser.init

        result <- runEitherT $ do
            printHeader "Stage1"
            (ast, astinfo) <- Pass.run1_ Stage1.pass src
            --ppPrint ast

            printHeader "SA"
            sa             <- Pass.run1_ SA.pass ast
            --ppPrint sa

            printHeader "Stage2"
            (ast, astinfo) <- Pass.run3_ Stage2.pass (Namespace [] sa) astinfo ast
            --ppPrint ast

            printHeader "ImplSelf"
            (ast, astinfo) <- Pass.run2_ ImplSelf.pass astinfo ast
            --ppPrint ast

            printHeader "SA"
            sa             <- Pass.run1_ SA.pass ast
            --ppPrint sa

            asdf <- Pass.run2_ TyChkr.pass astinfo ast
            ppPrint asdf

            return ()


        case result of
            Left  e -> putStrLn e
            Right _ -> return ()
        return ()

