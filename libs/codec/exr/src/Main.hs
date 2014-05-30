module Main where

import Control.Monad
import Data.Array.Repa as R
import System.Environment

import Flowbox.Codec.EXR



main :: IO ()
main = do
    (arg:_) <- getArgs
    Just ret <- openEXRFile arg

    readHeader ret >>= print

    (buf,_,_) <- readTileFromChannel' ret 0 "R"  (0, 0)
    --buf <- readTiledScanlineChannelR ret 0 "R"
    --forM_ [0..9] $ \index -> print $ buf `R.linearIndex` index

    dumpImageInfo ret
