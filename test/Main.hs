---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
import qualified Luna.Parser.Pragma as Pragma
import           Luna.Syntax.Name   (TName(TName))
import qualified Luna.Pass.Analysis.Struct as SA
import qualified Luna.Pass.Import          as I
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
import Control.Monad (when)
import Luna.Syntax.Name.Path (QualPath(QualPath))

import qualified Luna.Data.ModuleInfo as MI
import           Data.String.Utils (replace)


header txt = "\n-------- " <> txt <> " --------"
printHeader = putStrLn . header

ppPrint = putStrLn . ppShow

main = do
    args <- getArgs
    when (length args < 2) $ fail "provide input and output path!"
    let path   = args !! 0
        liFile = replace "luna" "" path
        out    = args !! 1
        src    = Source (QualPath [] "Main") (File $ fromString path)

    Session.runT $ do

        Parser.init

        Pragma.enable (Pragma.orphanNames)
        Pragma.pop    (Pragma.orphanNames)

        result <- runEitherT $ do
            printHeader "Stage1"
            (ast, astinfo) <- Pass.run1_ Stage1.pass src
            ppPrint ast
            -- This may be removed
            printHeader "Extraction of imports"
            ppPrint $ I.getImportList ast
            ppPrint $ I.getModPathsFromImportList . I.getImportList $ ast
            -- Up to this
            printHeader "SA"
            sa             <- Pass.run1_ SA.pass ast
            ppPrint sa
            let mInfo = MI.ModuleInfo [fromString liFile] sa
            liftIO $ MI.writeModInfoToFile mInfo
            --liftIO $ MI.writeStructInfoToFile "testModule.li" sa


            printHeader "Stage2"
            (ast, astinfo) <- Pass.run3_ Stage2.pass (Namespace [] sa) astinfo ast
            ppPrint ast

            printHeader "Imports"
            sa <- Pass.run

            printHeader "ImplSelf"
            (ast, astinfo) <- Pass.run2_ ImplSelf.pass astinfo ast
            ppPrint ast

            printHeader "SA"
            sa             <- Pass.run1_ SA.pass ast
            ppPrint sa

            printHeader "ImplScopes"
            (ast, astinfo) <- Pass.run3_ ImplScopes.pass astinfo sa ast
            ppPrint ast

            printHeader "ImplCalls"
            (ast, astinfo) <- Pass.run2_ ImplCalls.pass astinfo ast
            ppPrint ast

            --printHeader "Hash"
            --ast             <- Pass.run1_ Hash.pass ast

            printHeader "SSA"
            ast            <- Pass.run1_ SSA.pass ast
            ppPrint ast

            printHeader "HAST"
            hast           <- Pass.run1_ HASTGen.pass ast
            ppPrint hast

            printHeader "HSC"
            hsc            <- Pass.run1_ HSC.pass hast
            putStrLn (unpack hsc)
            liftIO $ writeFile out (unpack hsc)


        case result of
            Left  e -> putStrLn e
            Right _ -> return ()
        return ()


