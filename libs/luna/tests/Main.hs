---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

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

import qualified Flowbox.Luna.Codegen.Hs.Generator  as Gen
import qualified Flowbox.Luna.Codegen.Hs.AST.Module as Module

import qualified Flowbox.Luna.Parser                as Parser

import           Debug.Trace                          
import           Data.Either.Utils                    (forceEither)
import qualified Text.Show.Pretty                   as PP
import           System.TimeIt                        


example :: String
example = unlines [ "a+b"
                  ]


main :: IO ()
main = do
    timeIt main_inner


main_inner :: IO ()
main_inner = do
    let 
        parsed = Parser.parse example
        ast = forceEither parsed
        out = Gen.genModule ast
        a = f b
        b = g a

    --print out
    print a
    putStrLn ""
    putStrLn $ PP.ppShow $ parsed
    putStrLn "\n-----------------"
    putStrLn $ PP.ppShow $ out
    -- putStrLn $ Module.genCode out


f a = let 
    out = 0:a
    in out

g a = let
    out = 1:a
    in out 

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

