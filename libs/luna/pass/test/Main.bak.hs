---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}

--module Luna.Build.Build where

import Control.Monad.RWS hiding (mapM, mapM_, when)

import           Control.Monad.Trans.Either
import           Data.String.Utils                          (replace)
import           Data.Text.Lazy                             (pack, unpack)
import           System.FilePath                            ((</>), splitDirectories)
import           System.IO                                  (writeFile)
import qualified System.Environment                         as Env
import           Text.Show.Pretty                           (ppShow)

import           Flowbox.Control.Error
import           Flowbox.Prelude
import qualified Flowbox.System.Directory.Directory         as Directory
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Platform                    as Platform
import           Flowbox.System.UniPath                     (UniPath)
import qualified Flowbox.System.UniPath                     as UniPath
import qualified Luna.Data.ImportInfo                       as II
import qualified Luna.Data.ModuleInfo                       as ModInfo
import           Luna.Data.Namespace                        (Namespace (Namespace))
import qualified Luna.Data.Namespace                        as Namespace
import           Luna.Data.Source                           (Code (Code), Source (Source))
import qualified Luna.Data.Source                           as Source
import           Luna.Data.StructData                       (StructData (StructData))
import qualified Luna.Data.StructData                       as StructData
import qualified Luna.Parser.Parser                         as Parser
import qualified Luna.Parser.Pragma                         as Pragma
import qualified Luna.Pass                                  as Pass
import qualified Luna.Pass.Analysis.Imports                 as Imports
import qualified Luna.Pass.Transform.StdInsert              as StdInsert
import qualified Luna.Pass.Analysis.Struct                  as SA
import           Luna.Pass.Import                           (getImportPaths)
import qualified Luna.Pass.Import                           as Import
import qualified Luna.Pass.Target.HS.HASTGen                as HASTGen
import qualified Luna.Pass.Target.HS.HSC                    as HSC
import qualified Luna.Pass.Transform.Desugar.ImplicitCalls  as ImplCalls
import qualified Luna.Pass.Transform.Desugar.ImplicitScopes as ImplScopes
import qualified Luna.Pass.Transform.Desugar.ImplicitSelf   as ImplSelf
import qualified Luna.Pass.Transform.Parse.Stage1           as Stage1
import qualified Luna.Pass.Transform.Parse.Stage2           as Stage2
import qualified Luna.Pass.Transform.SSA                    as SSA
import           Luna.Syntax.Name.Path                      (QualPath (QualPath))
import qualified Luna.System.Pragma.Store                   as Pragma
import           Luna.System.Session                        as Session


import qualified Luna.Data.ModuleInfo                       as ModInfo
import qualified Luna.Syntax.Module                         as Module
import           Data.String.Utils                          (replace)
import           Data.List.Utils                            (split)





type Builder m = (MonadIO m, Functor m)

logger :: LoggerIO
logger = getLoggerIO $moduleName



srcFolder :: String
srcFolder = "src"


hsExt :: String
hsExt = ".hs"


cabalExt :: String
cabalExt = ".cabal"


tmpDirPrefix :: String
tmpDirPrefix = "lunac"

--runSession inclStd s =
--    case inclStd of
--        True  -> eitherStringToM . fst =<< Session.runT (void Parser.init >> Pragma.enable (Pragma.includeStd)  >> Pragma.enable (Pragma.orphanNames) >> runEitherT s)
--        False -> eitherStringToM . fst =<< Session.runT (void Parser.init >> Pragma.enable (Pragma.orphanNames) >> runEitherT s)

when_ test = when test . void

runSession inclStd s = do
    out <- Session.run $ do
        Parser.init
        when_ inclStd $ Pragma.enable (Pragma.includeStd)
        Pragma.enable (Pragma.orphanNames)
        runEitherT s
    eitherStringToM $ out

processCompilable rootSrc inclStd compilable  = do 
    let getBasePath = UniPath.toFilePath . UniPath.basePath . UniPath.fromFilePath
        rootPath    = getBasePath . getPath $ rootSrc ^. Source.src
        mkFile      = Source.File . pack . (rootPath </>) . (++ ".luna") . ModInfo.modPathToString
        sources     = map (\i -> Source i (mkFile i)) compilable
        hscs        = mapM (prepareSource inclStd rootSrc) sources
    hscs


--processSource src = parseSourceWithFun src src False (\_ _ _ -> return [])
    

--parseSource' rootSrc src inclStd = parseSourceWithFun rootSrc src inclStd processCompilable


parseSource rootSrc src inclStd = do
    printHeader "Stage1"
    (ast, astinfo) <- Pass.run1_ Stage1.pass src
    ppPrint ast

    let impPaths =  Import.getImportPaths ast
    compilable <- liftIO $ filterM ModInfo.moduleExists impPaths
    compiledCodes <- processCompilable rootSrc inclStd compilable

    (ast, astinfo) <- Pass.run2_ StdInsert.pass astinfo ast

    printHeader "Extraction of imports"
    importInfo     <- Pass.run1_ Imports.pass ast

    ----printHeader "Hash"
    ----ast             <- Pass.run1_ Hash.pass ast

    printHeader "SA"
    structData1 <- Pass.run2_ SA.pass (StructData mempty importInfo) ast
    let sa1     = structData1 ^. StructData.namespace . Namespace.info
        mInfo   = ModInfo.ModuleInfo liFile mempty sa1 mempty
        liFile  = src ^. Source.modName

    liftIO $ ModInfo.writeModInfoToFile mInfo (getPath  $ src ^. Source.src)
    ppPrint sa1

    printHeader "Stage2"
    (ast, astinfo) <- Pass.run3_ Stage2.pass (Namespace [] sa1) astinfo ast
    ppPrint ast

    printHeader "ImplSelf"
    (ast, astinfo) <- Pass.run2_ ImplSelf.pass astinfo ast
    ppPrint ast

    printHeader "SA2"
    structData2 <- Pass.run2_ SA.pass structData1 ast
    ppPrint (structData2 ^. StructData.namespace . Namespace.info)

    let sa2 = structData2 ^. StructData.namespace . Namespace.info
        ii2 = structData2 ^. StructData.importInfo

    printHeader "ImplScopes"
    (ast, astinfo) <- Pass.run2_ ImplScopes.pass (astinfo, sa2, ii2)  ast
    ppPrint ast

    printHeader "ImplCalls"
    (ast, _astinfo) <- Pass.run2_ ImplCalls.pass astinfo ast
    ppPrint ast
    return (ast, astinfo, importInfo, compiledCodes)


prepareSource :: Builder m => Bool -> Source Source.File -> Source Source.File  -> m [Source Code]
prepareSource inclStd rootSrc src = do
    codes <- runSession inclStd $ do
        (ast, astinfo, importInfo, compiledCodes) <- parseSource rootSrc src inclStd

        printHeader "SSA"
        ast            <- Pass.run1_ SSA.pass ast
        ppPrint ast

        printHeader "HAST"
        hast           <- Pass.run2_ HASTGen.pass importInfo ast
        ppPrint hast

        printHeader "HSC"
        hsc            <- Pass.run1_ HSC.pass hast
        putStrLn $ unpack hsc

        return (hsc, concat compiledCodes)
    return $ (Source (src ^. Source.modName) $ Code (fst codes)) : (snd codes)


header txt = "\n-------- " <> txt <> " --------"

printHeader :: (MonadIO m) => String -> m ()
printHeader = putStrLn . header

ppPrint :: (MonadIO m, Show a) => a -> m ()  
ppPrint = putStrLn . ppShow


main = do
    args <- Env.getArgs
    when (length args < 1) $ fail "Please provide input path!"
    let path     = args !! 0
        out      = args !! 1
        liPath   = replace ".luna" "" path
        pathSegs = splitDirectories liPath
        modName  = fromString $ last pathSegs
        filePath = map fromString $ init pathSegs
        src      = Source (QualPath [] modName) (Source.File $ fromString path)

    (processedSource:_) <- prepareSource False src src
    writeFile out (processedSource ^. Source.src ^. Source.code & unpack)

------------------------------------------------------------------------------------
-- CLASSES AND INSTANCES
------------------------------------------------------------------------------------
class GetFromSource a where
    getPath :: a -> String
    getText :: a -> String


instance GetFromSource Source.File where
    getPath (Source.File path) = unpack path
    getText (Source.File path) = "TODO"


instance GetFromSource Source.Text where
    getPath (Source.Text txt) = "Interactive/Interactive.luna"
    getText (Source.Text txt) = unpack txt

