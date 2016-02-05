---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Control.Concurrent                                                as Concurrent
import           Control.Monad                                                     (replicateM_)
import           Data.List                                                         (intercalate)
import           Text.RawString.QQ
import           Text.Show.Pretty

import qualified Flowbox.Batch.Project.Project                                     as Project
import qualified Flowbox.Config.Config                                             as Config
import           Flowbox.Control.Error
import           Flowbox.Data.Version                                              ()
import           Flowbox.Debug                                                     (timeit)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.UniPath                                            as UniPath
import           Flowbox.Text.Show.Hs                                              (hsShow)
import qualified Luna.DEP.AST.Control.Crumb                                        as Crumb
import qualified Luna.DEP.AST.Control.Focus                                        as Focus
import qualified Luna.DEP.AST.Control.Zipper                                       as Zipper
import           Luna.DEP.AST.Module                                               (Module)
import qualified Luna.DEP.AST.Module                                               as Module
import           Luna.DEP.AST.Name                                                 (Name (Name))
import qualified Luna.DEP.AST.Name                                                 as Name
import qualified Luna.DEP.AST.Type                                                 as Type
import           Luna.DEP.Data.Source                                              (Source (Source))
import qualified Luna.DEP.Data.Source                                              as Source
import           Luna.DEP.Lib.Lib                                                  (Library (Library))
import qualified Luna.DEP.Lib.Lib                                                  as Library
import           Luna.DEP.Lib.Manager                                              (LibManager)
import qualified Luna.DEP.Lib.Manager                                              as LibManager
import qualified Luna.DEP.Pass.Analysis.Alias.Alias                                as Analysis.Alias
import qualified Luna.DEP.Pass.Analysis.CallGraph.CallGraph                        as Analysis.CallGraph
import qualified Luna.DEP.Pass.CodeGen.HSC.HSC                                     as HSC
import qualified Luna.DEP.Pass.Transform.AST.DepSort.DepSort                       as Transform.DepSort
import qualified Luna.DEP.Pass.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
import qualified Luna.DEP.Pass.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
import qualified Luna.DEP.Pass.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
import qualified Luna.DEP.Pass.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt
import qualified Luna.DEP.Pass.Transform.AST.Hash.Hash                             as Hash
import qualified Luna.DEP.Pass.Transform.AST.SSA.SSA                               as SSA
import qualified Luna.DEP.Pass.Transform.AST.TxtParser.TxtParser                   as TxtParser
import qualified Luna.DEP.Pass.Transform.HAST.HASTGen.HASTGen                      as HASTGen
import qualified Luna.Interpreter.Session.AST.Executor                             as Executor
import qualified Luna.Interpreter.Session.Cache.Cache                              as Cache
import qualified Luna.Interpreter.Session.Cache.Invalidate                         as Invalidate
import qualified Luna.Interpreter.Session.Cache.Value                              as Value
import           Luna.Interpreter.Session.Data.CallPoint                           (CallPoint (CallPoint))
import           Luna.Interpreter.Session.Data.DefPoint                            (DefPoint (DefPoint))
import qualified Luna.Interpreter.Session.Env                                      as Env
import qualified Luna.Interpreter.Session.Error                                    as Error
import           Luna.Interpreter.Session.Memory.Manager.NoManager                 (NoManager (NoManager))
import qualified Luna.Interpreter.Session.Session                                  as Session
import qualified Luna.Interpreter.Session.TargetHS.Bindings                        as Bindings
import qualified Luna.Interpreter.Session.TargetHS.Reload                          as Reload



rootLogger :: Logger
rootLogger = getLogger ""


logger :: LoggerIO
logger = getLoggerIO $moduleName


code :: Source
code = Source ["Main"] $ [r|
class Vector a:
    x,y,z :: a
    def test a b:
        a,b

def print msg:
    ```autoLift1 print #{msg}```

def Int.add a:
    ```liftF2 (+) #{self} #{a}```

def + a b:
    a.add b

def > a b:
    a.gt b

def Int.gt a:
    ```liftF2 (>) #{self} #{a}```

def Int.inc:
    self + 1

def main:
    print $ 2 + 2.inc.inc
    print $ 1 > 2
    v = Vector 1 2 3
    print $ v
|]


code2 :: Source
code2 = Source ["Main"] $ [r|
class Vector a:
    x,y,z :: a
    def test a b:
        a,b

def print msg:
    ```autoLift1 print #{msg}```

def Int.add a:
    ```liftF2 (+) #{self} #{a}```

def + a b:
    a.add b

def > a b:
    a.gt b

def Int.gt a:
    ```liftF2 (>) #{self} #{a}```

def Int.inc:
    self + 1

def main:
    print $ 2 + 2.inc.inc
    print $ 3 > 2
    v = Vector 1 2 3
    print $ v
|]


readSource :: Source -> IO (LibManager, Library.ID)
readSource source = eitherStringToM' $ runEitherT $ do
    (ast, _, astInfo) <- EitherT $ TxtParser.run source
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitSelf.run astInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.TLRecUpdt.run astInfo ast
    aliasInfo         <- EitherT $ Analysis.Alias.run ast
    callGraph         <- EitherT $ Analysis.CallGraph.run aliasInfo ast
    ast               <- EitherT $ Transform.DepSort.run callGraph aliasInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitScopes.run astInfo aliasInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitCalls.run astInfo ast
    _aliasInfo        <- EitherT $ Analysis.Alias.run ast

    let path = UniPath.fromUnixString "."
    return $ LibManager.insNewNode (Library "Main" def path ast def astInfo) def


main1 :: IO ()
main1 = do
    rootLogger setIntLevel 5
    cfg <- Config.load

    (libManager , libID) <- readSource code
    (libManager2, _    ) <- readSource code2

    env <- Env.mk cfg NoManager libManager (Just $ Project.ID 0)
                (Just $ DefPoint libID [Crumb.Module "Main", Crumb.Function (Name.single "main") []])
                (curry $ curry $ curry print)

    putStrLn $ ppShow $ LibManager.lab libManager libID
    result <- Session.run cfg env [] $ do
        Env.addReload libID Reload.ReloadLibrary
        Executor.processMain_
        print =<< Value.getIfReady [CallPoint libID 92] 0.0
        putStrLn "--------- 1"
        Executor.processMain_
        putStrLn "========= 1"

        Cache.dumpAll
        Invalidate.modifyNode libID 92
        Cache.dumpAll

        Executor.processMain_
        putStrLn "--------- 2"
        Executor.processMain_

        putStrLn "========= ready ==========1="
        Cache.dumpAll
        Env.setLibManager libManager2
        Invalidate.modifyNode libID 92
        putStrLn "========= modified =======2="
        Cache.dumpAll
        putStrLn "========= running ========3="
        Executor.processMain_
        putStrLn "--------- 3"
        Env.setLibManager libManager
        Invalidate.modifyNode libID 92
        Executor.processMain_
        putStrLn "========= finished =======4="
        Cache.dumpAll

        print =<< Value.getIfReady [CallPoint libID 92] 0.0
    eitherStringToM $ fmapL Error.format result


main2 :: IO ()
main2 = do
    rootLogger setIntLevel 5

    (libManager , libID) <- readSource code
    let Just library = LibManager.lab libManager libID
        ast          = library ^. Library.ast
        astGet bc    = eitherStringToM
                     . fmap Zipper.getFocus
                     . Zipper.focusBreadcrumbs bc
                     . Zipper.mk
    putStrLn $ ppShow ast
    Just ast_main   <- Focus.getFunction <$> astGet [Crumb.Function (Name.single "main") []] ast
    Just ast_Vector <- Focus.getClass    <$> astGet [Crumb.Class    "Vector"   ] ast
    Just ast_IntAdd <- Focus.getFunction <$> astGet [Crumb.Function (Name.single "+") ["Int"]] ast

    logger info "Whole ast"
    printHsSrc ast
    logger info "main only"
    printHsSrc $ Module.methods .~ [ast_main] $ Module.mk 0 $ Type.Module 1 "Main" []
    logger info "Vector only"
    printHsSrc $ Module.classes .~ [ast_Vector] $ Module.mk 0 $ Type.Module 1 "Main" []
    logger info "Int.+ only"
    printHsSrc $ Module.methods .~ [ast_IntAdd] $ Module.mk 0 $ Type.Module 1 "Main" []
    logger info "empty"
    printHsSrc $ Module.mk 0 $ Type.Module 1 "Main" []


main3 :: IO ()
main3 = do
    rootLogger setIntLevel 5
    cfg <- Config.load

    (libManager , libID) <- readSource code

    env <- Env.mk cfg NoManager libManager (Just $ Project.ID 0)
                (Just $ DefPoint libID [Crumb.Module "Main", Crumb.Function (Name "main" []) []])
                (curry $ curry $ curry print)

    result <- Session.run cfg env [] $ do
        Session.setImports ["Foreign.Ptr", "Foreign.ForeignPtr", "System.Mem"]
        Session.runStmt "a <- newForeignPtr nullFunPtr nullPtr"
        Session.runStmt "performGC"
        liftIO $ Concurrent.threadDelay 3000000
        lift2 $ Bindings.remove "a"
        Session.runStmt "performGC"
        liftIO $ Concurrent.threadDelay 3000000
        Session.runStmt "print 4"
    eitherStringToM $ fmapL Error.format result


main4 :: IO ()
main4 = do
    rootLogger setIntLevel 0
    cfg <- Config.load

    (libManager , libID) <- readSource code

    env <- Env.mk cfg NoManager libManager (Just $ Project.ID 0)
                (Just $ DefPoint libID [Crumb.Module "Main", Crumb.Function (Name "main" []) []])
                (curry $ curry $ curry print)

    result <- Session.run cfg env [] $ do
        Session.setImports ["Prelude"]
        timeit "run1" $ replicateM_ 1 $ Session.runStmt "a <- return 1"
        timeit "run2" $ replicateM_ 1 $ Session.runStmt "b <- return a"
        timeit "run3" $ replicateM_ 1 $ Session.runStmt "b <- return b"
    eitherStringToM $ fmapL Error.format result

printHsSrc :: Module -> IO ()
printHsSrc ast = eitherStringToM' $ runEitherT $ do
    aliasInfo <- EitherT $ Analysis.Alias.run ast
    hash      <- EitherT $ Hash.run ast
    ssa       <- EitherT $ SSA.run aliasInfo hash
    hast      <- EitherT $ HASTGen.run ssa
    hsc       <- EitherT $ HSC.run  hast
    logger debug $ intercalate "\n\n" (map showSrc hsc)


showSrc :: Source -> String
showSrc src = ">>> file '" ++ intercalate "/" (src ^. Source.path) ++ "':\n\n"
             ++ hsShow (src ^. Source.code)

main :: IO ()
main = main1

--serialize parameters type
