module Flowbox.System.Console.ASCIISpinner where

import qualified Control.Concurrent as C
import           Control.Monad
import qualified System.IO          as IO

import Flowbox.Prelude hiding (elements)



progress :: Int -> IO ()
progress i = do
    let elements = "-\\|/"
        current  = elements !! (i `mod` length elements)
    putStr ('\b':[current])
    IO.hFlush IO.stdout


finish :: IO ()
finish = putStrLn "\b \b"


runWithSpinner :: IO a -> IO a
runWithSpinner fun = do
    putStr " "
    thread <- C.forkIO $ forM_ [0..] $ \i -> do progress i
                                                C.threadDelay 50000
    result <- fun
    C.killThread thread
    finish
    return result
