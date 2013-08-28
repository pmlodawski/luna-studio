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

import qualified Flowbox.Luna.Codegen.Hs.Generator     as Gen

import qualified Flowbox.Luna.Parser                   as Parser

import           Debug.Trace     
import           Data.Either.Utils                     (forceEither)
import qualified Text.Show.Pretty                      as PP
import           System.TimeIt   


example :: String
example = unlines [ "def f(x,y,z):\n x+y+z"
                  ]


main :: IO ()
main = do
    timeIt main_inner

main_inner :: IO ()
main_inner = do
    let 
        parsed = Parser.parse example
        ast = forceEither parsed
        out = Gen.convert ast

    --print out
    putStrLn $ PP.ppShow $ parsed
    putStrLn "-----------------"
    print ast


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

