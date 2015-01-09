module PrettyConsole (
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
colouredPrint fs x = do
    putStr $ "\x1b[" ++ fmt ++ "m"
    putStr x
    putStrLn "\x1b[0m"
  where fmt = intercalate ";" (fmap (show.attrtonum) fs)

data PrintAttrs = Black
                | Red
                | Green
                | Yellow
                | Blue
                | Magenta
                | Cyan
                | White
                | Bold

attrtonum :: PrintAttrs -> Int
attrtonum Black   = 30
attrtonum Red     = 31
attrtonum Green   = 32
attrtonum Yellow  = 33
attrtonum Blue    = 34
attrtonum Magenta = 35
attrtonum Cyan    = 36
attrtonum White   = 37
attrtonum Bold    = 1



--printer :: (Show a) => String -> a -> IO ()
--printer x y = printer_aux x (show y)

--printer_aux :: String -> String -> IO ()
--printer_aux x y = do  [Bold,White] `colouredPrint` "\n-----------------------------------------------------------------------------"
--                      putStr "> "
--                      [Yellow] `colouredPrint` x
--                      [Bold,White] `colouredPrint` "-----------------------------------------------------------------------------\n"
--                      putStrLn y

--section :: IO () -> IO ()
--section sec = do  sec
--                  [Bold, White] `colouredPrint` "\n\n#############################################################################\n\n"