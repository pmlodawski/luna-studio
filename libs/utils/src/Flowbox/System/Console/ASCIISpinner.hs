module Flowbox.System.Console.ASCIISpinner where

import qualified System.IO as IO

import Flowbox.Prelude hiding (elements)



progress :: Int -> IO ()
progress i = do
    let elements = ['-', '\\', '|', '/']
        current  = elements !! (i `mod` length elements)
    putStr ('\b':[current])
    IO.hFlush IO.stdout


finish :: IO ()
finish = do
    putStr "\b \b"
    IO.hFlush IO.stdout
