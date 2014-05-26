module Main where

import System.Environment

import Flowbox.Codec.EXR



main :: IO ()
main = do
    (arg:_) <- getArgs
    Just ret <- openEXRFile arg

    readHeader ret >>= print
    dumpImageInfo ret
