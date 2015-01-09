module Luna.Typechecker.Debug.PrettyConsole (
    PrintAttrs(..),
    colouredPrint,
    writeFileM
  ) where



import            Control.Monad.IO.Class                    (MonadIO, liftIO)
import            Data.List                                 (intercalate)



writeFileM :: (MonadIO m) => FilePath -> String -> m ()
writeFileM path str = liftIO $ do
    writeFile filepath str
    [Cyan] `colouredPrint` "…writing " ++ show filepath
  where filepath = "tmp/" ++ path


infix 4 `colouredPrint`
colouredPrint :: [PrintAttrs] -> String -> IO ()
colouredPrint fs x = putStrLn formatted_str
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
  show Black   = show 30
  show Red     = show 31
  show Green   = show 32
  show Yellow  = show 33
  show Blue    = show 34
  show Magenta = show 35
  show Cyan    = show 36
  show White   = show 37
  show Bold    = show 1

