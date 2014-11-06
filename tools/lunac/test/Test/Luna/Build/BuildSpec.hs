---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.Build.BuildSpec where

import           Control.Monad  (forM_)
import           Data.Version   (Version (Version))
import qualified System.Exit    as Exit
import qualified System.Process as Process
import           Test.Hspec

import qualified Flowbox.Config.Config    as Config
import           Flowbox.Prelude
import qualified Flowbox.System.Directory as Directory
import qualified Flowbox.System.IO.IO     as IO
import qualified Flowbox.System.UniPath   as UniPath
import qualified Luna.Build.Build         as Build
import qualified Luna.Build.Cmd           as Cmd
import           Test.Luna.Sample.Program (Program)
import qualified Test.Luna.Sample.Program as Program


main :: IO ()
main = hspec spec


build :: Program -> IO ()
build program =
    Directory.withTmpDirectory "" $ \tmpDir -> do
        cfg <- Config.load
        let inputFile  = UniPath.append "Main.luna" tmpDir
            outputFile = UniPath.append "out" tmpDir
        IO.writeFile inputFile (program ^. Program.code)
        Build.run cfg $ Cmd.BuildOptions
            {- input        -} (UniPath.toUnixString inputFile)
            {- output       -} (UniPath.toUnixString outputFile)
            {- optimisation -} 2
            {- link         -} []
            {- library      -} False
            {- libName      -} "testlib"
            {- libVersion   -} (Version [1] [])
            {- rootPath     -} ""
            {- global       -} False
            {- buildDir     -} ""
            {- ddebug       -} False
            {- dump_all     -} False
            {- dump_ast     -} False
            {- dump_aa      -} False
            {- dump_ssa     -} False
            {- dump_hash    -} False
            {- dump_hast    -} False
            {- dump_hsc     -} False
        (exitCode, stdout, stderr) <- Process.readProcessWithExitCode
            (UniPath.toUnixString outputFile) [] ""
        stdout `shouldBe` (program ^. Program.output)
        stderr `shouldBe` ""
        exitCode `shouldBe` Exit.ExitSuccess



spec :: Spec
spec = do
    describe "Luna Compiler" $ do
        describe "compiles sample programs" $ do
            forM_ Program.programs $ \program ->
                it ("compiles program " ++ show (program ^. Program.name)) $ do
                    build program
