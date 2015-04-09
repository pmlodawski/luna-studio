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
import qualified Luna.Parser.Parser                         as Parser
import           Text.PrettyPrint.ANSI.Leijen               (displayIO, linebreak, renderPretty)
import           Data.Default
import           Data.Either                                (lefts, rights)
import           System.IO                                  (stdout)
import           Text.Show.Pretty
import qualified Luna.Parser.State                          as State
import qualified Luna.Parser.State                          as ParserState

import qualified Luna.Data.ASTInfo                          as ASTInfo
import qualified Luna.Parser.Pragma                         as Pragma
import qualified Luna.Parser.Parser                         as Parser
import qualified Luna.Parser.Pragma                         as Pragma
import           Luna.Syntax.Name                           (TName(TName))
import qualified Luna.Pass.Analysis.Imports                 as Imports
import qualified Luna.Pass.Analysis.Struct                  as SA
import qualified Luna.Pass.Transform.Parse.Stage2           as Stage2
import qualified Luna.Pass.Transform.Parse.Stage1           as Stage1
import qualified Luna.Pass.Transform.Desugar.ImplicitSelf   as ImplSelf
import qualified Luna.Pass.Transform.Hash                   as Hash
import qualified Luna.Pass.Transform.SSA                    as SSA
import qualified Luna.Pass.Target.HS.HASTGen                as HASTGen
import qualified Luna.Pass.Target.HS.HSC                    as HSC
import qualified Luna.Pass.Transform.Desugar.ImplicitScopes as ImplScopes
import qualified Luna.Pass.Transform.Desugar.ImplicitCalls  as ImplCalls
import           Luna.Data.Namespace                        (Namespace(Namespace), _info)
import           Luna.Data.StructData                       (StructData(StructData), _namespace)
import qualified Luna.Pass                                  as Pass
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Class                  (lift)
import qualified Data.ByteString                            as ByteStr
import           Luna.Data.Source                           (Source(Source), File(File), Code(Code))
import qualified Luna.Data.Source                           as Source
import           Data.Text.Lazy                             (unpack)
import           Flowbox.Text.Show.Hs                       (hsShow)

import           Luna.System.Session                        as Session
import qualified Luna.System.Pragma.Store                   as Pragma
import           Control.Monad                              (when)
import           Luna.Syntax.Name.Path                      (QualPath(QualPath))

import qualified Luna.Data.ModuleInfo                       as MI
import           Data.String.Utils                          (replace)



header txt = "\n-------- " <> txt <> " --------"
printHeader = putStrLn . header

ppPrint = putStrLn . ppShow

main = do
    args <- getArgs
    when (length args < 2) $ fail "provide input and output path!"
    let path   = args !! 0
        liFile = replace ".luna" "" path
        out    = args !! 1
        src    = Source (QualPath [] (fromString liFile)) (File $ fromString path)

    Session.runT $ do

        Parser.init

        Pragma.enable (Pragma.orphanNames)
        -- Pragma.pop    (Pragma.orphanNames)

        result <- runEitherT $ do
            printHeader "Stage1"
            (ast, astinfo) <- Pass.run1_ Stage1.pass src
            ppPrint ast

            printHeader "Extraction of imports"
            importInfo <- Pass.run1_ Imports.pass ast 
            ppPrint importInfo

            printHeader "SA"
            sa           <- Pass.run2_ SA.pass (StructData mempty importInfo) ast
            let sa1 = _info . _namespace $ sa
            let mInfo = MI.ModuleInfo (QualPath [] (fromString liFile)) mempty mempty sa1 mempty
            ppPrint mInfo
            liftIO $ MI.writeModInfoToFile mInfo


            printHeader "Stage2"
            (ast, astinfo) <- Pass.run3_ Stage2.pass (Namespace [] sa1) astinfo ast
            ppPrint ast

            printHeader "ImplSelf"
            (ast, astinfo) <- Pass.run2_ ImplSelf.pass astinfo ast
            ppPrint ast

            printHeader "SA2"
            sa             <- Pass.run2_ SA.pass sa ast
            ppPrint sa

            printHeader "ImplScopes"
            (ast, astinfo) <- Pass.run3_ ImplScopes.pass astinfo sa1 ast
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


