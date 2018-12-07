{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Prelude hiding (FilePath)

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified PseudoMacros  as Macro
import qualified Shelly.Lifted as Shelly

import Control.Monad.IO.Class (liftIO)
import Data.Text              (Text)
import GHC.Exts               (toList)
import Shelly.Lifted          (FilePath, shelly)
import System.IO.Unsafe       (unsafePerformIO)


(</>) :: FilePath -> FilePath -> FilePath
(</>) = (Shelly.</>)

root :: FilePath
root = unsafePerformIO . shelly $ do
    let dirPathS   = List.dropWhileEnd (/= '/') $(Macro.__FILE__)
        dirPath    = Shelly.fromText $ Text.pack dirPathS
    Shelly.absPath $ dirPath </> ".." </> ".."


build_runner :: IO ()
build_runner = do
    putStrLn "• Building runner"
    runStack (root </> "runner") ["build"]

runStack :: FilePath -> [Text] -> IO ()
runStack path args = shelly $ do
    Shelly.chdir path $ Shelly.run "stack" args
    pure ()

main :: IO ()
main = do
    build_runner
