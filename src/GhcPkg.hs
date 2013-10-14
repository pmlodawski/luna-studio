{-# LANGUAGE OverloadedStrings #-}

module GhcPkg where

import qualified System.Cmd            as Cmd
import qualified System.Process        as Process
import qualified System.Environment    as Env
import qualified System.Exit           as Exit

import qualified Flowbox.Config.Config as Cfg
import           Data.String.Utils       (replace)
import System.IO
import Control.Monad


main :: IO ()
main = do
    cfg     <- Cfg.load
    args    <- Env.getArgs
    
    --appendFile "/tmp/test.txt" "-----------\n\n"
    --appendFile "/tmp/test.txt" $ "args: " ++ show args ++ "\n"

    stdinTxt <- if "-" `elem` args
                then do 
                     inTxt <- hGetContents stdin
                     out <- return $ if "update" `elem` args 
                        then replace ((Cfg.path . Cfg.ffs) cfg) "${pkgroot}/../../../.." inTxt
                        else inTxt
                     --appendFile "/tmp/test.txt" out
                     return out
                else return ""

    
    let exec = (Cfg.ghcPkgBin . Cfg.ghcTP . Cfg.thirdparty) cfg

    (exitCode, outResult, errResult) <- Process.readProcessWithExitCode 
                                        exec ( "--global-package-db"
                                        : (Cfg.pkgConf . Cfg.ghcTP . Cfg.thirdparty) cfg
                                        : "--global"
                                        : ("--package-db=" ++ (Cfg.pkgDb . Cfg.local) cfg)
                                        : args
                                        )
                                        stdinTxt
    hPutStr stderr errResult 
    putStr outResult
    Exit.exitWith exitCode
    
