---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

                        

--import qualified Flowbox.Luna.Samples.HelloWorld       as HelloWorld
--import qualified Flowbox.Luna.Codegen.Hs.FuncGenerator as FG
--import qualified Flowbox.Luna.Codegen.Hs.DefGenerator  as DG
--import qualified Flowbox.Luna.Codegen.Hs.CodeGenerator as CG
--import qualified Flowbox.Luna.Network.Def.DefManager   as DefManager

--import qualified Flowbox.Luna.Codegen.Hs.AST.Function  as Function
--import qualified Flowbox.Luna.Codegen.Hs.AST.Module    as Module

--import           Flowbox.Luna.Codegen.Hs.Cabal.Config    (Config)
--import qualified Flowbox.Luna.Codegen.Hs.Cabal.Config  as Config
--import qualified Flowbox.Luna.Codegen.Hs.Cabal.Section as Section

--import qualified Flowbox.Luna.Data.Graph               as Graph
--import qualified Flowbox.Luna.Lib.Library              as Library

--import qualified Flowbox.Luna.Builder.Builder          as Builder
--import qualified Flowbox.System.UniPath                as UniPath
--import           Flowbox.System.UniPath                  (UniPath)
----------------------
--import qualified Flowbox.Luna.Codegen.Hs.Generator  as Gen
--import qualified Flowbox.Luna.Codegen.Hs.AST.Module as Module

--import qualified Flowbox.Luna.Parser                as Parser

--import           Debug.Trace                          
--import           Data.Either.Utils                    (forceEither)
--import qualified Text.Show.Pretty                   as PP
--import           System.TimeIt                        


--example :: String
--example = unlines [ "a=1"
--                  ]


--main :: IO ()
--main = do
--    timeIt main_inner


--main_inner :: IO ()
--main_inner = do
--    let 
--        parsed = Parser.parse example
--        ast = forceEither parsed
--        out = Gen.genModule ast

--    --print out
--    putStrLn ""
--    putStrLn $ PP.ppShow $ parsed
--    putStrLn "\n-----------------"
--    putStrLn $ PP.ppShow $ out
--    -- putStrLn $ Module.genCode out


------------------------------------



import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.RWS 
import           Control.Monad.Trans.Either
import           System.Log.Logger       hiding (getLogger, setLevel, Logger, log)
import           Prelude                 hiding(log)

--nxt :: String -> State Int String
--nxt a = do
--    b <- get
--    put $ b+1
--    return a
 
--test :: State Int String
--test = do
--    b <- nxt "ala"
--    return b

--test :: StateT Int (Writer [String]) ()
--test = do
--    n <- get
--    tell ["o nie"]
--    tell ["o nie2"]
--    put $ succ n
--    return ()

data LogEntry = LogEntry { name     :: String
                         , priority :: Priority
                         , msg      :: String
                         } deriving (Show)


getLogger :: MonadWriter [LogEntry] m => String -> (String -> m()) -> m()
getLogger name = \f -> f name


log :: MonadWriter [LogEntry] m => Priority -> String -> String -> m()
log pri msg name = tell [LogEntry name pri msg]

debug :: MonadWriter [LogEntry] m => String -> String -> m()
debug = log DEBUG


logger = getLogger "Flowbox"

--test :: RWS Int [LogEntry] Int ()
test :: (Enum a, MonadState a m, MonadWriter [LogEntry] m) => EitherT String m ()
test = do
    n <- get
    logger.debug $ "o nie"
    left "err"
    put $ succ n
    return ()
 

main :: IO ()
main = do
    let y = runRWS (runEitherT test) 0 0
    print $ y
    return ()





--import           Control.Monad.State                  



--data GenState = GenState { varcount :: Int
--                         } deriving (Show)

--empty :: GenState
--empty = GenState 0


--nxt :: String -> State GenState String
--nxt a = do
--    b <- get
--    --put $ b+1
--    return a
 
--test :: State GenState String
--test = do
--    b <- genVarName
--    c <- genVarName
--    d <- genVarName
--    return d



--genVarName :: State GenState String
--genVarName = do
--    state <- get
--    let vname = "v''" ++ show (varcount state)
--    put $ state{varcount = 1 + varcount state}
--    return vname


 
--main :: IO ()
--main = do
--    print $ runState test empty
--    return ()





--main :: IO ()
--main = do 
--    --Parser.main
--    putStrLn "------------\n"
--    --putStrLn $ Module.genCode $ DG.generateDefinition HelloWorld.full_manager 100
--    --putStrLn $ Module.genCode $ CG.generateCommonCls "select0"


--    --let
--    --    builder = Builder.empty { Builder.path = UniPath.fromUnixString("samples/TestProject/build") }
--    --Builder.buildLibrary builder (HelloWorld.workspacelib)
--    return ()

