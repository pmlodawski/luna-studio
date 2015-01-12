module Luna.Typechecker.Debug.ConsoleColours (
    PrintAttrs(..),
    colouredPrint, writeFileM
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List              (intercalate)


writeFileM :: (MonadIO m) => FilePath -> String -> m ()
writeFileM path str = liftIO $ do
    writeFile filepath str
    [Cyan] `colouredPrint` "…writing " ++ show filepath
  where filepath = "tmp/" ++ path


infix 4 `colouredPrint`
colouredPrint :: (MonadIO m) => [PrintAttrs] -> String -> m ()
colouredPrint fs x = liftIO $ putStrLn formatted_str
  where fmt           = intercalate ";" $ fmap show fs
        formatted_str = "\x1b[" ++ fmt ++ "m" ++ x ++ "\x1b[0m"


data PrintAttrs = Black
                | Red
                | Green
                | Yellow
                | Blue
                | Magenta
                | Cyan
                | White
                | Bold


instance Show PrintAttrs where
  show Black   = show (30 :: Int)
  show Red     = show (31 :: Int)
  show Green   = show (32 :: Int)
  show Yellow  = show (33 :: Int)
  show Blue    = show (34 :: Int)
  show Magenta = show (35 :: Int)
  show Cyan    = show (36 :: Int)
  show White   = show (37 :: Int)
  show Bold    = show (1  :: Int)

