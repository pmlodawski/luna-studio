{-# LANGUAGE OverloadedStrings #-}

module GhcPkg where

import qualified System.Cmd            as Cmd
import qualified System.Process        as Process
import qualified System.Environment    as Env
import qualified System.Exit           as Exit

import qualified Flowbox.Config.Config as Cfg
import           Data.String.Utils       (replace)
import           System.IO               
import           Control.Monad           


main :: IO ()
main = do
    cfg     <- Cfg.load
    args    <- Env.getArgs
    let exec = (Cfg.ghcPkgBin . Cfg.ghcTP . Cfg.thirdparty) cfg

    appendFile "C:\\test.txt" "ghc-pkg\n"
    appendFile "C:\\test.txt" (show args)
    appendFile "C:\\test.txt" "\n---\n"

    --stdinTxt <- if "-" `elem` args
    --        then do 
    --             inTxt <- hGetContents stdin
    --             appendFile "C:/test.txt" inTxt
    --             return inTxt
    --        else return ""

    exitCode <- Cmd.rawSystem exec $ "--global-package-db"
                       : (Cfg.pkgConf . Cfg.ghcTP . Cfg.thirdparty) cfg
                       : "--global"
                       : ("--package-db=" ++ (Cfg.pkgDb . Cfg.global) cfg)
                       : ("--package-db=" ++ (Cfg.pkgDb . Cfg.local) cfg)
                       : args

    --(exitCode, outResult, errResult) <- Process.readProcessWithExitCode 
    --                                    exec ( "--global-package-db"
    --                                    : (Cfg.pkgConf . Cfg.ghcTP . Cfg.thirdparty) cfg
    --                                    : "--global"
    --                                    : ("--package-db=" ++ (Cfg.pkgDb . Cfg.global) cfg)
    --                                    : ("--package-db=" ++ (Cfg.pkgDb . Cfg.local) cfg)
    --                                    : args
    --                                    )
    --                                    stdinTxt

    Exit.exitWith exitCode


--main :: IO ()
--main = do
--    cfg     <- Cfg.load
--    args    <- Env.getArgs
    
--    stdinTxt <- if "-" `elem` args
--                then do 
--                     inTxt <- hGetContents stdin
--                     out <- return $ if "update" `elem` args && "--global" `elem` args
--                        then replace ((Cfg.path . Cfg.ffs) cfg) "${pkgroot}/../../../.." inTxt
--                        else inTxt
--                     --appendFile "/tmp/test.txt" out
--                     return out
--                else return ""

    
--    let exec = (Cfg.ghcPkgBin . Cfg.ghcTP . Cfg.thirdparty) cfg
--    print exec

--    (exitCode, outResult, errResult) <- Process.readProcessWithExitCode 
--                                        exec ( "--global-package-db"
--                                        : (Cfg.pkgConf . Cfg.ghcTP . Cfg.thirdparty) cfg
--                                        : "--global"
--                                        : ("--package-db=" ++ (Cfg.pkgDb . Cfg.local) cfg)
--                                        : args
--                                        )
--                                        stdinTxt
--    hPutStr stderr errResult 
--    putStr outResult
--    Exit.exitWith exitCode
    
